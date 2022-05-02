import { lazy } from "react";
import { Route, Routes } from "react-router-dom";

export default function BookablesPage() {
  const BookablesView = lazy(() => import("./BookablesView"));
  const BookableEdit = lazy(() => import("./BookableEdit"));
  const BookableNew = lazy(() => import("./BookableNew"));

  return (
    <Routes>
      <Route path="/:id" element={<BookablesView />} />
      <Route path="/" element={<BookablesView />} />
      <Route path="/:id/edit" element={<BookableEdit />} />
      <Route path="/new" element={<BookableNew />} />
    </Routes>
  );
}
