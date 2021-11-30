import { ChangeEvent, useEffect } from "react";
import { FaSpinner } from "react-icons/fa";
import User from "../../domain/User";
import useFetch from "../../utils/useFetch";
import { useUser } from "./UserContext";

export default function UserPicker() {
  const [user, setUser] = useUser();

  const {
    data: users = [],
    status,
    error,
  } = useFetch<User[]>("http://localhost:3001/users");

  useEffect(() => {
    setUser(users[0]);
  }, [users, setUser]);

  function handleSelect(e: ChangeEvent<HTMLSelectElement>) {
    const selectedID = parseInt(e.target.value, 10);
    const selectedUser = users?.find((u) => u.id === selectedID) ?? undefined;
    setUser(selectedUser);
  }

  if (error && status === "error") {
    return <p>{error.message}</p>;
  }

  if (status === "loading") {
    return <FaSpinner />;
  }

  return (
    <select className="user-picker" onChange={handleSelect} value={user?.id}>
      {users.map((u) => (
        <option key={u.id} value={u.id}>
          {u.name}
        </option>
      ))}
    </select>
  );
}
