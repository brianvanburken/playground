import {
  type FieldNode,
  type OperationDefinitionNode,
  parse,
  valueFromASTUntyped,
} from "graphql";
import { graphql, HttpResponse } from "msw";

export type Todo = {
  id: string;
  title: string;
  completed: boolean;
};

// elm-graphql inlines arguments into the document rather than sending GraphQL
// variables, so `variables` is always {} and the values have to come off the AST.
const rootFieldArgs = (query: string, field: string): Record<string, unknown> => {
  const operation = parse(query).definitions.find(
    (node): node is OperationDefinitionNode => node.kind === "OperationDefinition",
  );

  const selection = operation?.selectionSet.selections.find(
    (node): node is FieldNode => node.kind === "Field" && node.name.value === field,
  );

  return Object.fromEntries(
    (selection?.arguments ?? []).map((argument) => [
      argument.name.value,
      valueFromASTUntyped(argument.value),
    ]),
  );
};

// Resets on every page load, because MSW resolves handlers in the page rather
// than inside the service worker. Playwright gives each test a fresh page.
let todos: Todo[] = [
  { id: "seed-1", title: "Read the MSW docs", completed: false },
  { id: "seed-2", title: "Write a Playwright spec", completed: true },
];

const api = graphql.link("/graphql");

export const handlers = [
  api.query("Todos", () => {
    return HttpResponse.json({ data: { todos } });
  }),

  api.mutation("AddTodo", ({ query }) => {
    const { title } = rootFieldArgs(query, "addTodo") as { title: string };
    const todo: Todo = { id: crypto.randomUUID(), title, completed: false };
    todos.push(todo);
    return HttpResponse.json({ data: { addTodo: todo } });
  }),

  api.mutation("SetTodoCompleted", ({ query }) => {
    const { id, completed } = rootFieldArgs(query, "setTodoCompleted") as {
      id: string;
      completed: boolean;
    };
    const todo = todos.find((candidate) => candidate.id === id);

    if (!todo) {
      return HttpResponse.json({ errors: [{ message: `No todo with id ${id}` }] });
    }

    todo.completed = completed;
    return HttpResponse.json({ data: { setTodoCompleted: todo } });
  }),

  api.mutation("DeleteTodo", ({ query }) => {
    const { id } = rootFieldArgs(query, "deleteTodo") as { id: string };
    todos = todos.filter((candidate) => candidate.id !== id);
    return HttpResponse.json({ data: { deleteTodo: id } });
  }),
];
