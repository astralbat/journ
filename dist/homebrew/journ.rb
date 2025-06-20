class Journ < Formula
  desc     "…"
  homepage "https://github.com/astralbat/journ"
  url      "https://github.com/astralbat/journ/archive/refs/tags/v0.2.1-beta.11.tar.gz"
  # url    "https://github.com/astralbat/journ/archive/refs/tag/v0.2.1-beta.7.tar.gz"
  sha256   "…"
  license  "MIT"

  depends_on "rust"         => :build
  depends_on "python@3.12"  # fixed-version runtime

  def install
      # TODO: Use intall_name_tool to fix the Python library path as a post install action
    system "cargo", "install", "--locked", *std_cargo_args
  end
end

