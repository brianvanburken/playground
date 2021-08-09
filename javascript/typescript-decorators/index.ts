const NonEmtpyString = (
  target: Object,
  propertyKey: string,
  descriptor: PropertyDescriptor
) => {
  const originalMethod = descriptor.value;

  descriptor.value = function (...args: unknown[]) {
    if (args[0] === "") {
      console.log("String is empty");
    } else {
      originalMethod.apply(this, args);
    }
  };

  return descriptor;
};

class Check {
  public test = false;

  @NonEmtpyString
  public static checkString(str: string): void {
    console.log("String is not empty");
  }
}

Check.checkString("");
Check.checkString("non-empty");
