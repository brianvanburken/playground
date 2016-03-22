defmodule MathTest do
  use Assertion

  test "[SUCCESS] equal to or greater than", do: assert 1 >= 1
  test "[FAIL] equal to or greater than", do: assert -1 >= 0

  test "[SUCCESS] equal to or lesser than", do: assert 1 <= 1
  test "[FAIL] equal to or lesser than", do: assert 2 <= 1

  test "[SUCCESS] not equal to", do: assert 1 != 2
  test "[FAIL] not equal to", do: assert 1 != 1

  test "[SUCCESS] strict equal to", do: assert 1 === 1
  test "[FAIL] strict equal to", do: assert 1 === 1.0

  test "[SUCCESS] strict not equal to", do: assert 1 !== 2
  test "[FAIL] strict not equal to", do: assert 1 !== 1

  test "[SUCCESS] equal to", do: assert 1 == 1
  test "[FAIL] equal to", do: assert 1 == 2

  test "[SUCCESS] greater than", do: assert 2 > 1
  test "[FAIL] greater than", do: assert 1 > 2

  test "[SUCCESS] lesser to", do: assert 1 < 2
  test "[FAIL] lesser to", do: assert 2 < 1

  test "[SUCCESS] asserting boolean", do: assert true
  test "[FAIL] asserting boolean", do: assert false
end
