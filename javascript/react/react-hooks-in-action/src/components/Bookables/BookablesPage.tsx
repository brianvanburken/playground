import { Route, Routes } from "react-router-dom";
import BookablesEdit from "./BookablesEdit";
import BookablesNew from "./BookablesNew";
import BookablesView from "./BookablesView";

export default function BookablesPage() {
  return (
    <Routes>
      <Route path="/:id" element={<BookablesView />} />
      <Route path="/" element={<BookablesView />} />
      <Route path="/:id/edit" element={<BookablesEdit />} />
      <Route path="/new" element={<BookablesNew />} />
    </Routes>
  );
}
