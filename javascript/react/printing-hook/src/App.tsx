import { useEffect, useState } from "react";

function usePrinting() {
  const [isPrinting, setIsPrinting] = useState(false);

  useEffect(() => {
    const beforePrinting = () => setIsPrinting(true);
    const afterPrinting = () => setIsPrinting(false);

    window.addEventListener("beforeprint", beforePrinting);
    window.addEventListener("afterprint", afterPrinting);

    return () => {
      window.removeEventListener("beforeprint", beforePrinting);
      window.removeEventListener("afterprint", afterPrinting);
    };
  }, []);

  return isPrinting;
}

export default function App() {
  const isPrinting = usePrinting();

  return <span>{isPrinting ? "Printing" : "Not printing"}</span>;
}
