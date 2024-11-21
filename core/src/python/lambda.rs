/*
 * Copyright (c) 2022-2024. Mark Barrett
 * This file is part of Journ.
 * Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 * Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 * You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::err;
use crate::error::{JournError, JournResult};
use crate::ext::StrExt;
use crate::python::environment::{FromPyObjectOwned, PythonEnvironment};
use pyo3::ToPyObject;
use std::collections::HashMap;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    parameters: Vec<String>,
    expression: String,
}

impl Lambda {
    pub fn parameters(&self) -> &Vec<String> {
        &self.parameters
    }

    pub fn expression(&self) -> &str {
        &self.expression
    }

    /// Appends the code on to the end of the lambda and returns a new lambda.
    pub fn append(self, code: &str) -> Lambda {
        Lambda { parameters: self.parameters, expression: self.expression + code }
    }

    pub fn eval<'a, T>(
        &self,
        args: Vec<Box<dyn ToPyObject + 'a>>,
        journal_incarnation: u32,
    ) -> JournResult<T>
    where
        T: FromPyObjectOwned,
    {
        assert_eq!(args.len(), self.parameters.len());

        let mut locals = HashMap::new();
        for (p, a) in self.parameters().iter().zip(args) {
            locals.insert(p.to_string(), a);
        }
        PythonEnvironment::eval(self.expression(), Some(locals), Some(journal_incarnation))
    }

    pub fn run<'a>(
        &self,
        args: Vec<Box<dyn ToPyObject + 'a>>,
        journal_incarnation: Option<u32>,
    ) -> JournResult<()> {
        assert_eq!(args.len(), self.parameters.len(), "Arguments length mismatch");

        let mut locals = HashMap::new();
        for (p, a) in self.parameters().iter().zip(args) {
            locals.insert(p.to_string(), a);
        }
        PythonEnvironment::run(self.expression(), journal_incarnation, Some(locals), None)
    }

    pub fn run_and_return<'a, T>(
        &self,
        args: Vec<Box<dyn ToPyObject + 'a>>,
        return_var: &str,
        journal_incarnation: u32,
    ) -> JournResult<T>
    where
        T: FromPyObjectOwned,
    {
        assert_eq!(args.len(), self.parameters.len(), "Arguments length mismatch");

        let mut locals = HashMap::new();
        for (p, a) in self.parameters().iter().zip(args) {
            locals.insert(p.to_string(), a);
        }
        PythonEnvironment::run_then(
            self.expression(),
            Some(journal_incarnation),
            Some(locals),
            None,
            |_py, locals| {
                let local_val = locals.unwrap().get_item(return_var).ok().flatten().ok_or(err!(
                    "Could not find return variable: {} in the locals structure",
                    return_var
                ))?;
                T::extract(local_val)
                    .map_err(|e| err!("Unable to extract return result from lambda: {}", e))
            },
        )
    }
}

impl FromStr for Lambda {
    type Err = JournError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let arrow_pos = s.find("->");
        if let Some(arrow_pos) = arrow_pos {
            let parameters: Vec<String> = s[0..arrow_pos]
                .split(',')
                .map(|s| s.trim().to_string())
                .filter(|p| !p.is_empty())
                .collect();
            let expression = &s[arrow_pos + 2..];

            // Determine whether the expression's leading whitespace has a newline and if so, assume the initial
            // indent is to be just after the last newline before expr start. This way everything lines up. For example:
            // a ->
            //  if 1 == 1:
            //    print("true")
            // If not found, assume the leading whitespace to be the length of the parameters section and arrow,
            // filling with spaces. For example:
            // a -> if 1 == 1:
            //        print("true")
            let trimmed_expression = expression.trim_start();
            let mut expression = match expression[0..expression.len() - trimmed_expression.len()]
                .rfind(|c| c == '\n')
            {
                Some(pos) => expression[pos + 1..].to_string(),
                None => {
                    let mut expr = String::new();
                    let pos = arrow_pos + 2;
                    for c in s[0..pos].chars().rev() {
                        if c == '\n' || c == '\r' {
                            break;
                        }
                        expr.push(' ');
                    }
                    expr.push_str(expression);
                    expr
                }
            };

            let base_indent_amount =
                expression[0..expression.len() - expression.trim_start().len()].indented_amount();
            let mut outdented_expression = vec![];
            for (i, line) in expression.lines().enumerate() {
                match line.outdent(base_indent_amount) {
                    Some(outdented) => outdented_expression.push(outdented),
                    None => {
                        return Err(err!(
                            "Unable to outdent line {}, possible inconsistent indentation within expression: {}",
                            i + 1,
                            expression
                        ));
                    }
                }
            }
            expression = outdented_expression.join("\n");

            if expression.is_empty() {
                Err(err!("Cannot parse lambda: '{}': missing expression", s))
            } else {
                Ok(Lambda { parameters, expression })
            }
        } else {
            Err(err!("Cannot parse lambda: '{}': missing arrow", s))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::python::environment::PythonEnvironment;
    use crate::python::lambda::Lambda;
    use std::str::FromStr;

    #[test]
    fn test_indented_lambda() {
        PythonEnvironment::startup();

        // Newline after arrow. Expression has good indentation.
        let l1 = Lambda::from_str(
            r#"a -> 
            if True:
              a + 1
            a"#,
        )
        .unwrap();
        l1.run(vec![Box::new(1)], 1).unwrap();

        // No newline after arrow, expression indented from start of parameters.
        let l2 = Lambda::from_str(
            r#"
            a -> if True:
                   a + 1
                 a"#,
        )
        .unwrap();
        l2.run(vec![Box::new(1)], 1).unwrap();
    }
}
