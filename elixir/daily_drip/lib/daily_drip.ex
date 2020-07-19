defmodule DailyDrip do
  @input "./data/html/"
  @output "./data/markdown/"

  def run do
    File.mkdir_p(@output)
    @input
    |> File.ls!()
    |> Stream.map(fn x ->
      { x, Path.join(@input, x) |> File.read!() }
    end)
    |> Stream.map(fn {x, y} ->
      { x, extract_description(y) }
    end)
    |> Stream.map(fn {x, y} ->
      { x, html_to_markdown(y) }
    end)
    |> Stream.map(fn {filename, content} ->
      path = Path.join(@output, filename)
      File.write(path, content)
    end)
    |> Enum.to_list()
  end

  def extract_description(file) do
    file
    |> Floki.find(".drip-description")
    |> Floki.filter_out("div")
    |> Floki.raw_html()
  end

  def html_to_markdown(html) do
    html
    |> Pandex.html_to_commonmark()
    |> (fn
      {:ok, content} -> content
      _ -> ""
    end).()
    |> String.replace("``` highlight ", "```")
  end
end
