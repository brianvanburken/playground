defmodule Mime do
  @external_resource Path.join([__DIR__, "mimes.txt"])

  @doc """
  On using the module the options passed are stored in @options. Then we import
  the module so all functions are imported. As last step we run the @before_compile
  which will add the functions from the options and the mimes.txt file. Your can
  register custom mime types by passing it to the call:

  defmodule MyModule do
    use Mime, "text/my_mime_type", [ ".my_extension" ]
  end

  """
  defmacro __using__(options) do
    quote do
      @options unquote(options)
      import unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
    end
  end

  @doc """
  This function creates two list of mimes. One from the options and one from the
  external resource. The two lists are joined and then send to the compile
  function to create AST's for each item in the list.
  """
  defmacro __before_compile__(env) do
    custom_mimes = Module.get_attribute(env.module, :options) |> parse_options
    stream_mimes = Mime.__info__(:attributes)[:external_resource] |> parse_file
    Enum.concat(custom_mimes, stream_mimes) |> compile
  end

  @doc """
  This function receives the options passed when using this module. For each item
  it converts the key to a string from the Atom that it is. It returns a tuple
  for each options. The first item in the tuple is the type and the second one a
  list of extensions.

  """
  defp parse_options(options) do
    Enum.map options, fn({k,v}) -> {Atom.to_string(k), v} end
  end

  @doc """
  This function receives the path of the external resource. It loads in the file
  and splits contents into a list with tuples. The first item in the tuple is
  the type and the second one a list of extensions.
  """
  defp parse_file(path) do
    File.stream!(path) |> Enum.map(fn (line) ->
      [ type, rest ] = line |> String.split("\t") |> Enum.map(&String.strip/1)
      extensions = String.split(rest, ~r/,\s?/)
      { type, extensions }
    end)
  end

  @doc """
  This function receives a list of mimes and passes it to the defmapping function
  to build an AST of functions for each mime type and extension. These are then
  unquoted to define the functions. Fallback functions for not matching types or
  extension are added as well a function for checking if a type is valid.
  """
  defp compile(mimes) do
    mappings_ast = defmapping(mimes)
    quote do
      unquote(mappings_ast)
      def exts_from_type(_type), do: []
      def type_from_ext(_ext), do: nil
      def valid_type?(type), do: exts_from_type(type) |> Enum.any?
    end
  end

  @doc """
  For each item in the mimes list it defines the functions that fetches the
  extensions based on the mime type and type from extension. The result for a
  mime tuple like { "text/html", [ ".html" ] } results into the following
  functions:
  def exts_from_type("text/html"), do: [ ".html" ]
  def type_from_ext(".ext"), do: "text/html"
  Using pattern match this makes it fast and no runtime parsing is needed.
  """
  defp defmapping(mimes) do
    for {type, exts} <- mimes do
      quote do
        def exts_from_type(unquote(type)), do: unquote(exts)
        def type_from_ext(ext) when ext in unquote(exts), do: unquote(type)
      end
    end
  end
end

defmodule MimeMapper do
  use Mime, "text/emoji":  [ ".emj" ],
            "text/elixir": [ ".exs" ]
end

IO.inspect MimeMapper.exts_from_type("text/html")
IO.inspect MimeMapper.exts_from_type("text/emoji")
IO.inspect MimeMapper.valid_type?("text/elixir")
