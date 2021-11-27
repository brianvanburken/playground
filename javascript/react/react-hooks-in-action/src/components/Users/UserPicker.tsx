import { users } from "../../static.json";

export default function UserPicker() {
  return (
    <select>
      {users.map((u) => (
        <option>{u.name}</option>
      ))}
    </select>
  );
}
