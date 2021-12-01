import {
  QueryClient,
  useMutation,
  useQuery,
  useQueryClient,
} from "react-query";
import { useNavigate } from "react-router-dom";
import Bookable from "../../domain/Bookable";
import { deleteItem, editItem, getData } from "../../utils/api";

export function useBookable(id: string) {
  const queryClient = useQueryClient();
  return useQuery<Bookable>(
    ["bookable", id],
    () => getData(`http://localhost:3001/bookables/${id}`),
    {
      // refetching causes problems after deleting a bookable
      refetchOnWindowFocus: false,

      initialData: queryClient
        .getQueryData<Bookable[]>("bookables")
        ?.find((b) => id && b.id === parseInt(id, 10)),
    }
  );
}

export function useUpdateBookable() {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const mutation = useMutation<Bookable, Error, Bookable>(
    (item) => editItem(`http://localhost:3001/bookables/${item.id}`, item),
    {
      onSuccess: (bookable) => {
        updateBookablesCache(bookable, queryClient);

        queryClient.setQueryData(["bookable", String(bookable.id)], bookable);

        navigate(`/bookables/${bookable.id}`);
      },
    }
  );

  return {
    updateBookable: mutation.mutate,
    isUpdating: mutation.isLoading,
    isUpdateError: mutation.isError,
    updateError: mutation.error,
  };
}

export function updateBookablesCache(
  bookable: Bookable,
  queryClient: QueryClient
) {
  const bookables = queryClient.getQueryData<Bookable[]>("bookables") || [];
  const bookableIndex = bookables.findIndex((b) => b.id === bookable.id);

  if (bookableIndex !== -1) {
    bookables[bookableIndex] = bookable;
    queryClient.setQueryData("bookables", bookables);
  }
}

export function useDeleteBookable() {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const mutation = useMutation<Bookable, Error, Bookable>(
    (bookable) =>
      deleteItem<Bookable>(`http://localhost:3001/bookables/${bookable.id}`),
    {
      onSuccess: (_, bookable) => {
        const bookables =
          queryClient.getQueryData<Bookable[]>("bookables") || [];

        queryClient.setQueryData(
          "bookables",
          bookables.filter((b) => b.id !== bookable.id)
        );

        navigate(
          `/bookables/${getIdForFirstInGroup(bookables, bookable) || ""}`
        );
      },
    }
  );

  return {
    deleteBookable: mutation.mutate,
    isDeleting: mutation.isLoading,
    isDeleteError: mutation.isError,
    deleteError: mutation.error,
  };
}

function getIdForFirstInGroup(
  bookables: Bookable[],
  excludedBookable: Bookable
) {
  // get the id and group of the deleted bookable
  const { id, group } = excludedBookable;

  // find the first other bookable in the same group as the deleted one
  const bookableInGroup = bookables.find(
    (b) => b.group === group && b.id !== id
  );

  // return its id or undefined
  return bookableInGroup?.id;
}
