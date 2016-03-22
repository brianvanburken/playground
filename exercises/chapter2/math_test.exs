defmodule MathTest do
  use Assertion

  test "assert [SUCCESS] equal to or greater than", do: assert 1 >= 1
  test "assert [FAIL] equal to or greater than", do: assert -1 >= 0

  test "assert [SUCCESS] equal to or lesser than", do: assert 1 <= 1
  test "assert [FAIL] equal to or lesser than", do: assert 2 <= 1

  test "assert [SUCCESS] not equal to", do: assert 1 != 2
  test "assert [FAIL] not equal to", do: assert 1 != 1

  test "assert [SUCCESS] strict equal to", do: assert 1 === 1
  test "assert [FAIL] strict equal to", do: assert 1 === 1.0

  test "assert [SUCCESS] strict not equal to", do: assert 1 !== 2
  test "assert [FAIL] strict not equal to", do: assert 1 !== 1

  test "assert [SUCCESS] equal to", do: assert 1 == 1
  test "assert [FAIL] equal to", do: assert 1 == 2

  test "assert [SUCCESS] greater than", do: assert 2 > 1
  test "assert [FAIL] greater than", do: assert 1 > 2

  test "assert [SUCCESS] lesser to", do: assert 1 < 2
  test "assert [FAIL] lesser to", do: assert 2 < 1

  test "assert [SUCCESS] asserting boolean", do: assert true
  test "assert [FAIL] asserting boolean", do: assert false


  test "refute [FAIL] equal to or greater than", do: refute 1 >= 1
  test "refute [SUCCESS] equal to or greater than", do: refute -1 >= 0

  test "refute [FAIL] equal to or lesser than", do: refute 1 <= 1
  test "refute [SUCCESS] equal to or lesser than", do: refute 2 <= 1

  test "refute [FAIL] not equal to", do: refute 1 != 2
  test "refute [SUCCESS] not equal to", do: refute 1 != 1

  test "refute [FAIL] strict equal to", do: refute 1 === 1
  test "refute [SUCCESS] strict equal to", do: refute 1 === 1.0

  test "refute [FAIL] strict not equal to", do: refute 1 !== 2
  test "refute [SUCCESS] strict not equal to", do: refute 1 !== 1

  test "refute [FAIL] equal to", do: refute 1 == 1
  test "refute [SUCCESS] equal to", do: refute 1 == 2

  test "refute [FAIL] greater than", do: refute 2 > 1
  test "refute [SUCCESS] greater than", do: refute 1 > 2

  test "refute [FAIL] lesser to", do: refute 1 < 2
  test "refute [SUCCESS] lesser to", do: refute 2 < 1

  test "refute [FAIL] refuteing boolean", do: refute true
  test "refute [SUCCESS] refuteing boolean", do: refute false
end
