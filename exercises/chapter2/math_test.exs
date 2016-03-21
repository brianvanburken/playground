defmodule MathTest do
  use Assertion

  test "equal to or greater than" do
    assert 1 >= 1
    assert -1 >= 0
  end

  test "equal to or lesser than" do
    assert 1 <= 1
    assert 2 <= 1
  end

  test "not equal to" do
    assert 1 != 2
    assert 1 != 1
  end

  test "strict equal to" do
    assert 1 === 1
    assert 1 === 1.0
  end

  test "strict not equal to" do
    assert 1 !== 2
    assert 1 !== 1.0
  end

  test "equal to" do
    assert 1 == 1
    assert 1 == 2
  end

  test "greater than" do
    assert 2 > 1
    assert 1 > 2
  end

  test "lesser to" do
    assert 1 < 2
    assert 2 < 1
  end
end
