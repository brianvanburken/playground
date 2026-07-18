export namespace Elm {
  namespace Main {
    interface Flags {
      count: number;
    }

    interface App {
      ports: {
        saveCount: {
          subscribe(callback: (count: number) => void): void;
          unsubscribe(callback: (count: number) => void): void;
        };
      };
    }

    function init(options: { node?: HTMLElement | null; flags: Flags }): App;
  }
}
