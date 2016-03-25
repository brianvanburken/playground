defmodule Translator do

  defmacro __using__(_options) do
    quote do
      Module.register_attribute __MODULE__, :locales, accumulate: true,
                                                      persist: false
      import unquote(__MODULE__), only: [locale: 2]
      @before_compile unquote(__MODULE__)
    end
  end

  defmacro __before_compile__(env) do
    compile(Module.get_attribute(env.module, :locales))
  end

  defmacro locale(name, mappings) do
    quote bind_quoted: [name: name, mappings: mappings] do
      @locales {name, mappings}
    end
  end

  def compile(translations) do
    translations_ast = for {locale, source} <- translations do
      deftranslations(locale, "", source)
    end

    quote do
      def t(locale, path, binding \\ [])
      unquote(translations_ast)

      # These two methods are used for pluralization
      def t(locale, path, [ count: count ]) when count == 1, do: t(locale, "#{path}.one")
      def t(locale, path, [ count: count ]), do: t(locale, "#{path}.other")

      def t(_locale, _path, _bindings), do: {:error, :no_translation}
    end
  end

  defp deftranslations(locale, current_path, translations) do
    for {key, val} <- translations do
      path = append_path(current_path, key)
      if Keyword.keyword?(val) do
        deftranslations(locale, path, val)
      else
        quote do
          def t(unquote(locale), unquote(path), bindings) do
            unquote(interpolate(val))
          end
        end
      end
    end
  end

  defp interpolate(string) do
    ~r/(?<head>)%{[^}]+}(?<tail>)/
    |> Regex.split(string, on: [:head, :tail])
    |> Enum.reduce("", fn
      <<"%{" <> rest>>, acc ->
        key = String.to_atom(String.rstrip(rest, ?}))
        quote do
          unquote(acc) <> to_string(Dict.fetch!(bindings, unquote(key)))
        end
      segment, acc -> quote do: (unquote(acc) <> unquote(segment))
    end)
  end

  defp append_path("", next), do: to_string(next)
  defp append_path(current, next), do: "#{current}.#{next}"
end

defmodule I18n do
  use Translator

  locale "en",
    flash: [
      hello: "Hello %{first} %{last}!",
      bye: "Bye, %{name}!"
    ],
    users: [
      title: "Users",
    ],
    plural: [
      user: [
        one: "user",
        other: "users",
      ],
    ]

  locale "fr",
    flash: [
      hello: "Salut %{first} %{last}!",
      bye: "Au revoir, %{name}!"
    ],
    users: [
      title: "Utilisateurs",
    ]
end

IO.inspect I18n.t("en", "flash.hello", first: "Chris", last: "McCord")
IO.inspect I18n.t("en", "plural.user", count: 1)
IO.inspect I18n.t("en", "plural.user", count: 2)
