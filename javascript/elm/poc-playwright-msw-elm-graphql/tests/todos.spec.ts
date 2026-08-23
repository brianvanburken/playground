import { expect, test } from "@playwright/test";

test.beforeEach(async ({ page }) => {
  await page.goto("/");
  // Generous only here: the first navigation after codegen pays for Vite
  // transforming the generated Api.* tree, which can outlast the 5s default.
  await expect(page.getByRole("listitem")).toHaveCount(2, { timeout: 15_000 });
});

test("renders the seeded todos", async ({ page }) => {
  await expect(page.getByText("Read the MSW docs")).toBeVisible();
  await expect(page.getByRole("checkbox", { name: "Toggle Read the MSW docs" })).not.toBeChecked();
  await expect(
    page.getByRole("checkbox", { name: "Toggle Write a Playwright spec" }),
  ).toBeChecked();
});

test("adds a todo", async ({ page }) => {
  await page.getByRole("textbox", { name: "New todo" }).fill("Ship the spike");
  await page.getByRole("button", { name: "Add" }).click();

  await expect(page.getByText("Ship the spike")).toBeVisible();
  await expect(page.getByRole("listitem")).toHaveCount(3);
  await expect(page.getByRole("textbox", { name: "New todo" })).toHaveValue("");
});

test("toggles a todo", async ({ page }) => {
  const checkbox = page.getByRole("checkbox", { name: "Toggle Read the MSW docs" });

  await checkbox.click();
  await expect(checkbox).toBeChecked();

  await checkbox.click();
  await expect(checkbox).not.toBeChecked();
});

test("deletes a todo", async ({ page }) => {
  await page.getByRole("button", { name: "Delete Read the MSW docs" }).click();

  await expect(page.getByText("Read the MSW docs")).toBeHidden();
  await expect(page.getByRole("listitem")).toHaveCount(1);
});
