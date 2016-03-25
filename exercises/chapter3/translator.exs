defmodule Translator do

  @doc """
  This function is triggered on `use Translator`. It takes the registers a
  module variable which accumilates everything for each definition. This way we
  can use it in multiple modules and all get the same translattions. After that
  the module is imported into the module that uses it. As last step it triggers
  the before_compile function that eventually compiles all the translations into
  methods.
  """
  defmacro __using__(_options) do
    quote do
      Module.register_attribute __MODULE__, :locales, accumulate: true,
                                                      persist: false
      import unquote(__MODULE__), only: [locale: 2]
      @before_compile unquote(__MODULE__)
    end
  end

  @doc """
  This function passes the locales module attribute to the compile function
  which will process it further.
  """
  defmacro __before_compile__(env) do
    compile(Module.get_attribute(env.module, :locales))
  end

  @doc """
  This function is called to accumulate the locales. The first parameter is an
  identifier of the locale. This can be an ISO code like EN for English
  translations. The second paramter contains a mapping of all the locales. The
  keys define the pad. The locales are placed on the module attribute locales
  and gets accumulated globally as defined in the __using__ function.
  """
  defmacro locale(name, mappings) do
    quote bind_quoted: [name: name, mappings: mappings] do
      @locales {name, mappings}
    end
  end

  @doc """
  It compiles all the translations into AST and uses that to define the
  translation function on the modules. This function defines the t/3 helper
  function for translating.
  """
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

  @doc """
  This function defines all the translation functions for each translation that
  is accumulated. The nested keys are used as a path with which the developer
  can fetch a translation. So a map with the key `:flash` that contains a map
  with a key `:hello` turns into `flash.hello` as a path. If the value of a key
  contains a string the function is defined and the value is interpolated if it
  contains any placeholders.
  """
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

  @doc """
  The string passed to this function gets interpolated so the placeholders can
  be replaced with their actual values. A placeholder is defined as `%{name}`.
  For a string like `"Hello %{name}!"` gets turned into the following Elixir
  code: `"" <> "Hello " <> Dict.fetch(bindings, :name) <> "!"`. This is an
  optimisation done in compile time so it does not have to interpet the
  placeholders on runtime.
  """
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

  @doc """
  These functions build up the path for the translations.
  """
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
IO.inspect I18n.t("en", "plural.unkown", count: 2)
