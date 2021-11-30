import { useContext, useState } from "react";
import User from "../../domain/User";
import UserContext from "./UserContext";
import UserDetails from "./UserDetails";
import UsersList from "./UsersList";

export default function UsersPage() {
  const [user, setUser] = useState<User>();

  const contextUser = useContext(UserContext);
  const currentUser = user || contextUser;

  return (
    <main className="users-page">
      <UsersList user={currentUser} setUser={setUser} />
      <UserDetails user={currentUser} />
    </main>
  );
}
