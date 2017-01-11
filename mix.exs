defmodule Pqueue.Mixfile do
  use Mix.Project

  def project do
    [app: :pqueue,
     version: "1.6.0",
     description: description(),
     package: package(),
     deps: deps()]
  end

  defp deps do
    []
  end

  defp description do
    "Erlang Priority Queue Implementation"
  end

  defp package do
    [files: ~w(src doc test rebar.config README.markdown),
     maintainers: ["Michael Truog"],
     licenses: ["BSD"],
     links: %{"GitHub" => "https://github.com/okeuday/pqueue"}]
   end
end
