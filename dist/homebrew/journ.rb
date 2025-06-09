class Journ < Formula
  desc     "…"
  homepage "https://github.com/astralbat/journ"
  url      "https://github.com/astralbat/journ/archive/refs/tags/v0.2.0.tar.gz"
  sha256   "…"
  license  "MIT"

  depends_on "rust"         => :build
  depends_on "python@3.12"  # fixed-version runtime

  def install
    system "cargo", "install", "--locked", *std_cargo_args
  end
end

