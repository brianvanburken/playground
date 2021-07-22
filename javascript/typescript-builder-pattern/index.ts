type Builder<T, B = Record<string, unknown>> = {
  // Take each key from T and define a function that takes an argument of the
  // same type as the property in T which returns an instance of the builder
  [K in keyof T & string as `with${Capitalize<K>}`]: (
    arg: T[K]
    // "B & Record<K, T[K]>" merges the generic record with a record that contains
    // the property with the same type as the interface
  ) => Builder<T, B & Record<K, T[K]>>;
} & {
  // Add build property that returns T in case B fully matches the interface.
  // Else it will return "never". This enforces completion
  build: B extends T ? () => T : never;
};

function Builder<T>(): Builder<T> {
  const state: Record<string, unknown> = {};

  const Builder = new Proxy(
    {},
    {
      get(_target, prop: string) {
        // If property is "build" we return our internal state to finish
        // the builder.
        if (prop === "build") {
          return () => state;
        }

        // Remove "with" and uncapitalize property
        prop = prop.charAt(4).toLowerCase() + prop.slice(5);

        return (x: unknown): Builder<T> => {
          state[prop.toString()] = x;
          return Builder as Builder<T>;
        };
      },
    }
  );

  return Builder as Builder<T>;
}

// ----- usage

interface A {
  prop1: string;
  prop2: number;
  prop3?: string[];
}

const a = Builder<A>()
  .withProp1("test")
  .withProp2(2)
  .withProp3(undefined)
  .withProp3([])
  .withProp3([""])
  .build();

console.log(a);

// The following line fails since it is not complete and thus returns "never"
// const b = Builder<A>().withProp1("test").build();

const partial = Builder<A>().withProp1("test");
const c = partial.withProp2(0).build();
