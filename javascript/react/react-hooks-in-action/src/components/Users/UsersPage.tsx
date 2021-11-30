import { useState } from "react";
import User from "../../domain/User";
import { useUser } from "./UserContext";
import UserDetails from "./UserDetails";
import UsersList from "./UsersList";

export default function UsersPage() {
  const [user, setUser] = useState<User>();

  const [contextUser] = useUser();
  const currentUser = user || contextUser;

  return (
    <main className="users-page">
      <UsersList user={currentUser} setUser={setUser} />
      <UserDetails user={currentUser} />
    </main>
  );
}
