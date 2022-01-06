#---
# Excerpted from "Metaprogramming Elixir",
# published by The Pragmatic Bookshelf.
# Copyrights apply to this code. It may not be used to create training material, 
# courses, books, articles, and the like. Contact us if you are in doubt.
# We make no guarantees that this code is fit for any purpose. 
# Visit http://www.pragmaticprogrammer.com/titles/cmelixir for more book information.
#---
defmodule MathTest do
  use Assertion
  test "integers can be added and subtracted" do
    assert 2 + 3 == 5
    assert 5 - 5 == 10
  end
  test "integers can be multiplied and divided" do
    assert 5 * 5 == 25
    assert 10 / 2 == 5
  end
end
