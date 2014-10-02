defmodule Pqueue.Mixfile do
  use Mix.Project

  def project do
    [app: :pqueue,
     version: "1.3.3",
     description: description,
     package: package,
     deps: deps]
  end

  defp deps do
    []
  end

  defp description do
    "Erlang Priority Queue Implementation"
  end

  defp package do
    [files: ~w(src doc test Makefile rebar.config README.markdown),
     contributors: ["Michael Truog"],
     licenses: ["BSD"],
     links: %{"GitHub" => "https://github.com/okeuday/pqueue"}]
   end
end
