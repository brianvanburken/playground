import { ChangeEvent, useContext, useEffect, useState } from "react";
import { FaSpinner } from "react-icons/fa";
import User from "../../domain/User";
import UserContext, { UserSetContext } from "./UserContext";

export default function UserPicker() {
  const [users, setUsers] = useState<User[]>();
  const user = useContext(UserContext);
  const setUser = useContext(UserSetContext);

  useEffect(() => {
    (async () => {
      const response = await fetch("http://localhost:3001/users");
      const users: User[] = await response.json();
      setUsers(users);
      setUser(users[0]);
    })();
  }, [setUser]);

  function handleSelect(e: ChangeEvent<HTMLSelectElement>) {
    const selectedID = parseInt(e.target.value, 10);
    const selectedUser = users?.find((u) => u.id === selectedID) ?? undefined;
    setUser(selectedUser);
  }

  if (!users) {
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
