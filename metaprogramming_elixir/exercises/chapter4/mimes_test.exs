Code.require_file("assertion.exs", __DIR__)
Code.require_file("mimes.exs", __DIR__)

defmodule MimeTest do
  use Assertion

  defmodule MimeMapper do
    use Mime, "text/emoji":  [ ".emj" ],
              "text/elixir": [ ".exs" ]
  end

  test "that the extension is loaded from file" do
    assert MimeMapper.exts_from_type("text/html") == [".html"]
  end

  test "fetching mime type from extension" do
    assert MimeMapper.type_from_ext(".html") == "text/html"
  end

  test "setting custom mimes" do
    assert MimeMapper.exts_from_type("text/elixir") == [".exs"]
  end

  test "setting mime type" do
    assert MimeMapper.type_from_ext(".exs") == "text/elixir"
  end
end

MimeTest.run
