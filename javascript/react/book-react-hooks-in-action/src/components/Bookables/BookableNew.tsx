import { useMutation, useQueryClient } from "react-query";
import { useNavigate } from "react-router-dom";
import Bookable from "../../domain/Bookable";
import { createItem } from "../../utils/api";
import PageSpinner from "../UI/PageSpinner";
import BookableForm from "./BookableForm";
import useFormState from "./useFormState";

export default function BookableNew() {
  const navigate = useNavigate();
  const formState = useFormState();
  const queryClient = useQueryClient();
  const {
    mutate: createBookable,
    status,
    error,
  } = useMutation<Bookable, Error, Bookable>(
    (item) => createItem<Bookable>("http://localhost:3001/bookables", item),
    {
      onSuccess: (bookable) => {
        queryClient.setQueryData<Bookable[]>("bookables", (old) => [
          ...(old || []),
          bookable,
        ]);
        navigate(`/bookables/${bookable.id}`);
      },
    }
  );

  function handleSubmit() {
    if (formState.state) {
      createBookable(formState.state);
    }
  }

  if (error && status === "error") {
    return <p>{error.message}</p>;
  }

  if (status === "loading") {
    return <PageSpinner />;
  }

  return <BookableForm formState={formState} handleSubmit={handleSubmit} />;
}
