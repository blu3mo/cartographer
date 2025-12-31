import { expect, test } from "@playwright/test";

test("homepage loads successfully", async ({ page }) => {
  await page.goto("/");

  // Wait for the page to be visible
  await expect(page).toHaveURL(/.*localhost:3000/);
});
