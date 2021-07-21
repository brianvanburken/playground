interface A {
  propA: string;
}

interface B {
  propB: string;
}

interface C {
  propC: string;
}

function isB(b: object): b is B {
  return "propB" in b;
}

function isC(c: object): c is C {
  return "propC" in c;
}

class AMapper {
  public static map(a: B): A;
  public static map(a: C): A;
  public static map(arg: object): A | null {
    if (isB(arg)) {
      return { propA: arg.propB };
    } else if (isC(arg)) {
      return { propA: arg.propC };
    }
    return null;
  }
}

const b: B = { propB: "propB" };

console.log(AMapper.map(b));

const c: C = { propC: "propC" };

console.log(AMapper.map(c));

// The following fails because it is neither B nor C
// AMapper.map({});
