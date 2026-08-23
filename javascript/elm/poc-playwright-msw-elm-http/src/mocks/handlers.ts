import { http, HttpResponse } from "msw";

export type Todo = {
  id: string;
  title: string;
  completed: boolean;
};

// Resets on every page load, because MSW resolves handlers in the page rather
// than inside the service worker. Playwright gives each test a fresh page.
let todos: Todo[] = [
  { id: "seed-1", title: "Read the MSW docs", completed: false },
  { id: "seed-2", title: "Write a Playwright spec", completed: true },
];

export const handlers = [
  http.get("/api/todos", () => {
    return HttpResponse.json(todos);
  }),

  http.post("/api/todos", async ({ request }) => {
    const { title } = (await request.json()) as { title: string };
    const todo: Todo = { id: crypto.randomUUID(), title, completed: false };
    todos.push(todo);
    return HttpResponse.json(todo, { status: 201 });
  }),

  http.patch("/api/todos/:id", async ({ params, request }) => {
    const { completed } = (await request.json()) as { completed: boolean };
    const todo = todos.find((candidate) => candidate.id === params.id);

    if (!todo) {
      return new HttpResponse(null, { status: 404 });
    }

    todo.completed = completed;
    return HttpResponse.json(todo);
  }),

  http.delete("/api/todos/:id", ({ params }) => {
    todos = todos.filter((candidate) => candidate.id !== params.id);
    return new HttpResponse(null, { status: 204 });
  }),
];
