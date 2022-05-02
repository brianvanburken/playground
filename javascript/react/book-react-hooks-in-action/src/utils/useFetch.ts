import { useEffect, useState } from "react";
import { getData } from "./api";

export type FetchStatus = "idle" | "loading" | "success" | "error";

export default function useFetch<T>(url: string) {
  const [data, setData] = useState<T>();
  const [error, setError] = useState<Error>();
  const [status, setStatus] = useState<FetchStatus>("idle");

  useEffect(() => {
    let doUpdate = true;
    setStatus("loading");
    setData(undefined);
    setError(undefined);

    getData<T>(url)
      .then((data: T) => {
        if (doUpdate) {
          setData(data);
          setStatus("success");
        }
      })
      .catch((error: Error) => {
        if (doUpdate) {
          setError(error);
          setStatus("error");
        }
      });

    return () => {
      doUpdate = false;
    };
  }, [url]);

  return { data, status, error };
}
