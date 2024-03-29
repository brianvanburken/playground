Code.require_file("html_entities.exs", __DIR__)
Code.require_file("html_formatter.exs", __DIR__)

defmodule Html do
  import HtmlEntities, only: [ encode: 1 ]
  import HtmlFormatter, only: [ format: 1 ]

  @external_resource tags_path = Path.join([__DIR__, "tags.txt"])
  @tags (for line <- File.stream!(tags_path, [], :line) do
    line |> String.strip |> String.to_atom
  end)

  defmacro markup(do: block) do
    quote do
      {:ok, var!(buffer, Html)} = start_buffer([])
      unquote(Macro.postwalk(block, &postwalk/1))
      result = render(var!(buffer, Html))
      :ok = stop_buffer(var!(buffer, Html))
      result
    end
  end

  def postwalk({:text, _meta, [string]}) do
    quote do: put_buffer(var!(buffer, Html), encode(to_string(unquote(string))))
  end
  def postwalk({tag_name, _meta, [[do: inner]]}) when tag_name in @tags do
    quote do: tag(unquote(tag_name), [], do: unquote(inner))
  end
  def postwalk({tag_name, _meta, [attrs, [do: inner]]}) when tag_name in @tags do
    quote do: tag(unquote(tag_name), unquote(attrs), do: unquote(inner))
  end
  def postwalk(ast), do: ast

  def start_buffer(state), do: Agent.start_link(fn -> state end)

  def stop_buffer(buff), do: Agent.stop(buff)

  def put_buffer(buff, content), do: Agent.update(buff, &[content | &1])

  def render(buff), do: Agent.get(buff, &(&1)) |> Enum.reverse |> format

  defmacro tag(name, attrs \\ [], do: inner) do
    quote do
      put_buffer var!(buffer, Html), open_tag(unquote_splicing([name, attrs]))
      unquote(postwalk(inner))
      put_buffer var!(buffer, Html), unquote("</#{name}>")
    end
  end

  def open_tag(name, []), do: "<#{name}>"
  def open_tag(name, attrs) do
    attr_html = for {key, val} <- attrs, into: "", do: " #{key}=\"#{val}\""
    "<#{name}#{attr_html}>"
  end
end


defmodule Template do
  import Html

  def render do
    markup do
      div id: "main" do
        h1 class: "title" do
          text "Welcome!"
        end
      end
      div class: "row" do
        div do
          text "XSS Protection <script>alert('vulnerable?');</script>"
        end
        p do: text "Hello!"
      end
    end
  end
end

IO.write "\n\n"
IO.write Template.render
IO.write "\n\n"


