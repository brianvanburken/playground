import { useEffect, useState } from "react";
import { FaSpinner } from "react-icons/fa";
import User from "../../domain/User";

export default function UserPicker() {
  const [users, setUsers] = useState<User[] | null>(null);

  useEffect(() => {
    (async () => {
      const response = await fetch("http://localhost:3001/users");
      const users: User[] = await response.json();
      setUsers(users);
    })();
  }, []);

  if (users === null) {
    return <FaSpinner />;
  }

  return (
    <select>
      {users.map((u) => (
        <option key={u.id}>{u.name}</option>
      ))}
    </select>
  );
}
