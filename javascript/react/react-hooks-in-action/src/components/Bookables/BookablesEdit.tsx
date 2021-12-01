import { FaSpinner } from "react-icons/fa";
import { useParams } from "react-router-dom";
import BookableForm from "./BookableForm";
import {
  useBookable,
  useDeleteBookable,
  useUpdateBookable,
} from "./bookableHooks";
import useFormState from "./useFormState";

export default function BookablesEdit() {
  const { id } = useParams();
  const { data, isLoading } = useBookable(id ?? "");
  const formState = useFormState(data);

  const { updateBookable, isUpdating, isUpdateError, updateError } =
    useUpdateBookable();

  const { deleteBookable, isDeleting, isDeleteError, deleteError } =
    useDeleteBookable();

  function handleDelete() {
    if (
      formState.state &&
      window.confirm("Are you sure you want to delete the bookable?")
    ) {
      deleteBookable(formState.state);
    }
  }

  function handleSubmit() {
    if (formState.state) {
      updateBookable(formState.state);
    }
  }

  if (isUpdateError || isDeleteError) {
    return <p>{updateError?.message || deleteError?.message}</p>;
  }

  if (isLoading || isUpdating || isDeleting) {
    return <FaSpinner />;
  }

  return (
    <BookableForm
      formState={formState}
      handleSubmit={handleSubmit}
      handleDelete={handleDelete}
    />
  );
}
