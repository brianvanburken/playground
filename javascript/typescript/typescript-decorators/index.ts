function isNonEmptyString(a: string): boolean {
  return a !== "";
}

function minLength(a: string, len: number): boolean {
  return a.length >= len;
}

function arg<T>(
  index: number,
  fn: (a: T) => boolean
): (args: unknown[]) => boolean {
  return function (args: unknown[]): boolean {
    return fn(args?.[index] as T);
  };
}

function args<F extends (...args: any) => boolean>(
  fn: F
): (args: unknown[]) => boolean {
  return function (args: unknown[]): boolean {
    return fn(...args);
  };
}

type ArgFunction = (args: unknown[]) => boolean;

function Guard(...validators: ArgFunction[]) {
  return (
    _target: Object,
    _propertyKey: string,
    descriptor: PropertyDescriptor
  ) => {
    const originalMethod = descriptor.value;

    descriptor.value = function (...args: unknown[]) {
      if (validators.every((fn) => fn.bind(this)(args))) {
        originalMethod.apply(this, args);
      } else {
        console.info("Guard did not pass");
      }
    };

    return descriptor;
  };
}

class Check {
  public test = false;

  @Guard(arg<string>(0, isNonEmptyString), args(minLength))
  public static staticCheckString(str: string, len: number): void {
    console.log(`String is not empty, "${str}", and is of length ${len}`);
  }
}

Check.staticCheckString("", 99);
Check.staticCheckString("non-empty", 99);
Check.staticCheckString("non-empty string of length", 10);
