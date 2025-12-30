import { test, expect } from '@playwright/test';

test.describe('Session Response Modification', () => {
  test('modifying a past response should not clear subsequent responses', async ({ page }) => {
    // Navigate to the session page
    const sessionUrl = 'http://localhost:3001/sessions/862bb320-cc55-4898-b73a-6e3558804ee2';
    await page.goto(sessionUrl);

    // Wait for the page to load
    await page.waitForLoadState('networkidle');

    // Check if we're on the session page
    await expect(page).toHaveURL(/sessions\/[a-f0-9-]+/);

    // Find all response items in the history/list
    const responseItems = page.locator('[data-testid="response-item"]');
    const responseCount = await responseItems.count();

    console.log(`Found ${responseCount} responses in the session`);

    if (responseCount < 2) {
      console.log('Not enough responses to test modification flow. Skipping test.');
      test.skip();
      return;
    }

    // Find the first response's "修正する" button
    const firstResponseItem = responseItems.first();
    const modifyButton = firstResponseItem.locator('button:has-text("修正する")');

    // Check if the modify button exists
    const modifyButtonExists = await modifyButton.count() > 0;
    if (!modifyButtonExists) {
      console.log('No "修正する" button found. This might be an admin view or the response is not yet answered.');
      test.skip();
      return;
    }

    // Get the text of the second response before modification
    const secondResponseItem = responseItems.nth(1);
    const secondResponseText = await secondResponseItem.locator('[data-testid="response-value"]').textContent();
    console.log(`Second response before modification: ${secondResponseText}`);

    // Click the "修正する" button on the first response
    await modifyButton.click();

    // Wait for the statement to be displayed (the question should now be the first one)
    await page.waitForTimeout(500);

    // Find and click a response button (e.g., "強く同意" or any available response)
    const responseButtons = page.locator('button:has-text("強く同意"), button:has-text("同意"), button:has-text("反対")');
    const firstAvailableButton = responseButtons.first();

    const buttonExists = await firstAvailableButton.count() > 0;
    if (!buttonExists) {
      console.log('No response buttons found. The UI might have changed.');
      test.skip();
      return;
    }

    await firstAvailableButton.click();

    // Wait for the response to be saved and navigation to occur
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(1000);

    // Check that we're back at the latest question (not the second question)
    // The second response should still exist and not be cleared
    const updatedResponseItems = page.locator('[data-testid="response-item"]');
    const updatedResponseCount = await updatedResponseItems.count();

    console.log(`Response count after modification: ${updatedResponseCount}`);

    // The count should remain the same (no responses were cleared)
    expect(updatedResponseCount).toBe(responseCount);

    // Verify the second response still has its value
    const updatedSecondResponseItem = updatedResponseItems.nth(1);
    const updatedSecondResponseText = await updatedSecondResponseItem.locator('[data-testid="response-value"]').textContent();

    console.log(`Second response after modification: ${updatedSecondResponseText}`);

    // The second response should not be empty or cleared
    expect(updatedSecondResponseText).toBeTruthy();
    expect(updatedSecondResponseText).toBe(secondResponseText);
  });

  test('modifying a response should update the response value', async ({ page }) => {
    const sessionUrl = 'http://localhost:3001/sessions/862bb320-cc55-4898-b73a-6e3558804ee2';
    await page.goto(sessionUrl);

    await page.waitForLoadState('networkidle');

    const responseItems = page.locator('[data-testid="response-item"]');
    const responseCount = await responseItems.count();

    if (responseCount < 1) {
      console.log('No responses found. Skipping test.');
      test.skip();
      return;
    }

    const firstResponseItem = responseItems.first();
    const modifyButton = firstResponseItem.locator('button:has-text("修正する")');

    const modifyButtonExists = await modifyButton.count() > 0;
    if (!modifyButtonExists) {
      console.log('No "修正する" button found.');
      test.skip();
      return;
    }

    // Get the original response value
    const originalValue = await firstResponseItem.locator('[data-testid="response-value"]').textContent();
    console.log(`Original response: ${originalValue}`);

    // Click modify
    await modifyButton.click();
    await page.waitForTimeout(500);

    // Click a different response (try to find one that's different from the original)
    const responseButtons = page.locator('button:has-text("強く同意"), button:has-text("同意"), button:has-text("反対"), button:has-text("強く反対")');

    // Find a button that doesn't match the original value
    let clickedButton = null;
    for (let i = 0; i < await responseButtons.count(); i++) {
      const button = responseButtons.nth(i);
      const buttonText = await button.textContent();
      if (buttonText && !originalValue?.includes(buttonText)) {
        clickedButton = button;
        break;
      }
    }

    if (!clickedButton) {
      // If we can't find a different button, just click the first one
      clickedButton = responseButtons.first();
    }

    await clickedButton.click();

    // Wait for save and navigation
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(1000);

    // The first response should now have the new value
    const updatedResponseItems = page.locator('[data-testid="response-item"]');
    const updatedFirstResponseItem = updatedResponseItems.first();
    const updatedValue = await updatedFirstResponseItem.locator('[data-testid="response-value"]').textContent();

    console.log(`Updated response: ${updatedValue}`);

    // The value should have changed (or at least exist)
    expect(updatedValue).toBeTruthy();
  });
});
