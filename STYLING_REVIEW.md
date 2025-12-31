# CRITICAL FRONTEND STYLING REVIEW
## Cartographer Project

---

## EXECUTIVE SUMMARY

The frontend has **significant styling inconsistencies** that undermine the design system implementation. While a well-structured CSS variable system exists in `globals.css`, it is **largely ignored** throughout the codebase in favor of hardcoded Tailwind colors and magic values. The `client-page.tsx` file is especially problematic with 2,800+ lines containing deeply nested, repetitive styling patterns.

**Key Issues:**
- Over 150+ instances of hardcoded Tailwind colors (slate, red, blue, amber, etc.)
- Inconsistent spacing patterns and border radius values
- Design system colors (CSS variables) underutilized
- Long, unreadable className strings (some 200+ characters)
- Dark mode handled inconsistently with inline color specifications
- Magic numbers and arbitrary values throughout
- Over-engineering with too many utility combinations

---

## 1. DESIGN SYSTEM VIOLATIONS

### 1.1 Hardcoded Colors Instead of CSS Variables

**Problem:** The tailwind config defines a proper color system using CSS variables, but components ignore it in favor of direct Tailwind colors.

**Evidence:**

#### File: `/Users/tkgshn/Developer/plural-reality/cartographer/app/sessions/[sessionId]/[accessToken]/client-page.tsx`

**Lines 188-262:** Report status metadata uses hardcoded colors:
```tsx
const REPORT_STATUS_META: Record<SessionReportStatus, { label: string; dot: string; text: string }> = {
  pending: {
    label: "待機中",
    dot: "bg-amber-500 dark:bg-amber-400",  // ❌ HARDCODED
    text: "text-amber-700 dark:text-amber-400",  // ❌ HARDCODED
  },
  generating: {
    dot: "bg-sky-500 dark:bg-sky-400",  // ❌ HARDCODED
    text: "text-sky-700 dark:text-sky-400",  // ❌ HARDCODED
  },
  completed: {
    dot: "bg-emerald-500 dark:bg-emerald-400",  // ❌ HARDCODED
    text: "text-emerald-700 dark:text-emerald-400",  // ❌ HARDCODED
  },
  failed: {
    dot: "bg-rose-500 dark:bg-rose-400",  // ❌ HARDCODED
    text: "text-rose-700 dark:text-rose-400",  // ❌ HARDCODED
  },
};
```

**Why It's a Problem:**
- Uses 8+ arbitrary colors not defined in the design system
- Dark mode values hardcoded inline rather than relying on CSS variables
- If design changes, these must be updated manually in multiple places
- Breaks the theming system - should use primary, secondary, destructive colors from design system

**Lines 221-264:** Report templates with excessive color definitions:
```tsx
const REPORT_TEMPLATES: ReportTemplate[] = [
  {
    color: "bg-pink-100 border-pink-200 hover:bg-pink-200 text-pink-900 dark:bg-pink-950/30 dark:border-pink-800 dark:hover:bg-pink-950/50 dark:text-pink-300",  // ❌ 11 color values in one string!
  },
  {
    color: "bg-blue-100 border-blue-200 hover:bg-blue-200 text-blue-900 dark:bg-blue-950/30 dark:border-blue-800 dark:hover:bg-blue-950/50 dark:text-blue-300",  // ❌ Repeated pattern
  },
  {
    color: "bg-purple-100 border-purple-200 hover:bg-purple-200 text-purple-900 dark:bg-purple-950/30 dark:border-purple-800 dark:hover:bg-purple-950/50 dark:text-purple-300",  // ❌ Repeated pattern
  },
  {
    color: "bg-slate-100 border-slate-200 hover:bg-slate-200 text-slate-900 dark:bg-slate-800 dark:border-slate-700 dark:hover:bg-slate-700 dark:text-slate-300",
  },
];
```

**Lines 1159, 1247, 1250, 1301, 1305, 1318:** Direct text color specifications:
```tsx
<h1 className="text-3xl font-semibold text-slate-900 dark:text-slate-100">  // ❌ Should use text-foreground
<p className="text-[10px] uppercase tracking-[0.2em] text-slate-400 dark:text-gray-500">  // ❌ Mixed slate + gray, hardcoded
<p className="text-sm font-semibold text-slate-900 dark:text-gray-100">  // ❌ Mixed slate + gray
<Heart className="h-4 w-4 text-pink-600" />  // ❌ Icon color hardcoded
<Brain className="h-4 w-4 text-blue-600" />  // ❌ Icon color hardcoded
<ChevronDown className="h-3.5 w-3.5 text-slate-400" />  // ❌ Icon color hardcoded
```

**Lines 1322, 1332, 1344:** Background colors hardcoded:
```tsx
<div className="rounded-3xl border border-slate-200/70 bg-slate-50/80 px-4 py-3 text-xs text-slate-500">  // ❌ All colors hardcoded
<div className="absolute inset-0 h-full w-full bg-slate-950/70 backdrop-blur-sm">  // ❌ Modal overlay hardcoded
<div className="relative z-10 w-full max-w-2xl rounded-3xl border border-slate-200 bg-white p-8 shadow-2xl">  // ❌ Modal bg hardcoded
```

**Lines 1487, 1510, 1530, 1550:** Input and text areas:
```tsx
<div className="invisible absolute right-0 top-6 z-10 w-80 rounded-xl border border-slate-200 bg-white p-3 text-xs leading-relaxed text-slate-700 shadow-lg opacity-0 transition-all group-hover:visible group-hover:opacity-100">  // ❌ Tooltip hardcoded
<textarea className="w-full resize-none rounded-xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-300">  // ❌ Should use Input component or card colors
<div className="min-h-[200px] whitespace-pre-wrap rounded-xl border border-slate-200 bg-white px-4 py-3 text-sm leading-relaxed text-slate-700">  // ❌ Multiple instances of same styling
```

### 1.2 Mixing slate and gray colors inconsistently

**Lines 1247, 1250:** 
```tsx
<p className="text-[10px] uppercase tracking-[0.2em] text-slate-400 dark:text-gray-500">  // ❌ Uses gray-500 in dark mode
<p className="text-sm font-semibold text-slate-900 dark:text-gray-100">  // ❌ Uses gray-100 in dark mode
```

**Why It's a Problem:**
- Gray and slate are different color families - this breaks visual consistency
- The design system only uses `hsl` variables - there is no gray color defined
- This will look wrong if dark mode CSS variables change

### 1.3 Status/State Colors Not Using Design System

**Lines 1584, 1843:** Warning/info states:
```tsx
<div className="rounded-2xl border border-amber-200 bg-amber-50 px-4 py-3 text-sm text-amber-700">  // ❌ Hardcoded warning state
<div className="rounded-2xl border border-red-200/70 bg-red-50/70 px-4 py-4 dark:border-red-900/70 dark:bg-red-950/40">  // ❌ Hardcoded error state
```

**Should be:** Using `destructive` or a new `warning` color system token

### 1.4 Inconsistent Dark Mode Handling

**Lines 2238-2242:**
```tsx
<div className="rounded-2xl border border-red-200/70 bg-red-50/70 px-4 py-4 dark:border-red-900/70 dark:bg-red-950/40">
  <p className="text-sm font-medium text-red-700 dark:text-red-400">
    <p className="mt-1 text-xs text-red-600 dark:text-red-400/90">
```

**Problem:** Dark mode colors hardcoded inline. Should rely on CSS variables defined in `:root` and `.dark` selector.

---

## 2. INCONSISTENT SPACING PATTERNS

### 2.1 Padding Inconsistencies

The codebase uses multiple padding values for similar components:
- `p-2` (8px)
- `p-3` (12px)
- `p-4` (16px)
- `p-6` (24px)
- `p-8` (32px)

**Lines 1217, 1344, 1387, 1472, 1486, etc.:**
```tsx
<CardFooter className="flex justify-end border-t border-slate-100 pt-4 dark:border-white/20">  // ❌ pt-4
<div className="relative z-10 w-full max-w-2xl rounded-3xl border border-slate-200 bg-white p-8 shadow-2xl">  // ✅ p-8 (modal - justified)
<div className="rounded-lg bg-white p-2 shadow-sm">  // ❌ p-2 (inconsistent icon container)
<div className="sticky top-0 space-y-3 rounded-2xl border border-slate-200 bg-slate-50 p-4">  // ❌ p-4
<div className="invisible absolute right-0 top-6 z-10 w-80 rounded-xl border border-slate-200 bg-white p-3 text-xs leading-relaxed text-slate-700 shadow-lg opacity-0 transition-all group-hover:visible group-hover:opacity-100">  // ❌ p-3
```

**Issue:** No clear pattern. Similar components use different padding values.

### 2.2 Gap Inconsistencies

Multiple gap values used for similar layouts:
- `gap-1.5`
- `gap-2`
- `gap-2.5`
- `gap-3`
- `gap-4`
- `gap-6`
- `gap-8`

**Lines 1177, 1207, 1226, 1257, 1279, 1369, 1371, 1590, 1695, etc.:**
```tsx
<div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">  // ❌ gap-4
<div className="grid gap-2.5 sm:grid-cols-2 xl:grid-cols-3">  // ❌ gap-2.5 (why different from above?)
<Button className="gap-2">  // ❌ gap-2 (button spacing)
<CardContent className="space-y-6">  // ❌ space-y-6
<div className="flex items-center gap-3">  // ❌ gap-3
<div className="flex gap-4">  // ❌ gap-4
<div className="space-y-3">  // ❌ space-y-3
```

**Problem:** No semantic meaning to gaps. A design system should define:
- `gap-tight` (for compact layouts)
- `gap-normal` (for standard layouts)
- `gap-relaxed` (for spacious layouts)

---

## 3. INCONSISTENT BORDER RADIUS

### 3.1 Multiple Border Radius Values

The codebase mixes:
- `rounded` (no value - 0.25rem default)
- `rounded-md` (0.375rem)
- `rounded-lg` (0.5rem)
- `rounded-xl` (0.75rem)
- `rounded-2xl` (1rem)
- `rounded-3xl` (1.5rem)
- `rounded-full` (9999px)

**Lines showing the inconsistency:**
```tsx
<div className="rounded-lg bg-white p-2 shadow-sm">  // Line 1387 - rounded-lg for icon container
<div className="rounded-2xl border..." >  // Line 1377 - rounded-2xl for button container
<div className="rounded-3xl border...">  // Line 1322, 1344 - rounded-3xl for larger containers
<div className="rounded-xl border...">  // Line 1486, 1510, 1530 - rounded-xl for tooltips/inputs
<span className="rounded-full border...">  // Line 1627 - rounded-full for badges
```

**Problem:** No clear hierarchy. For a semantic design system:
- Inputs/form elements should use `rounded-md`
- Cards should use `rounded-lg`
- Modals/popovers should use `rounded-xl`
- Badges should be `rounded-full`
- Buttons should be consistent with cards or inputs

The current mix suggests different designers worked on different parts without coordination.

---

## 4. OVER-ENGINEERING & COMPLEX CLASSNAMES

### 4.1 Excessively Long Utility Strings

**Line 1277-1279:**
```tsx
<Button
  className="min-h-[52px] w-full justify-center gap-3 rounded-2xl px-5 pl-32 pr-32 text-center text-base shadow-lg shadow-slate-900/10 sm:text-lg"
>
```

**Problems:**
1. Too many utilities: 13 utilities in one string
2. `pl-32 pr-32` is a hack - should use `text-center` + flex or proper padding
3. `shadow-lg shadow-slate-900/10` - double shadow definition (first is layer, second is color)
4. `min-h-[52px]` - arbitrary value (should be `h-12` or better yet, size="lg"`)
5. Mixing responsive (`sm:text-lg`) with static styling

### 4.2 Repeated Styling Patterns

**Lines 1377-1414-1445:** This same pattern repeats 3+ times:
```tsx
className={`flex w-full items-start gap-4 rounded-2xl border p-4 text-left transition ${
  selectedTemplate === template.id
    ? "border-purple-400 bg-purple-50"
    : "border-slate-200 bg-white hover:border-slate-300 hover:bg-slate-50"
}`}
```

**This should be extracted to a reusable component:**
```tsx
<TemplateCard 
  template={template}
  isSelected={selectedTemplate === template.id}
  onClick={() => setSelectedTemplate(template.id)}
/>
```

### 4.3 Arbitrary Values Instead of Design Tokens

**Line 1510:**
```tsx
className="w-full resize-none rounded-xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-300"
```

This should just use the `Input` component that's already defined!

**Line 1486:**
```tsx
className="invisible absolute right-0 top-6 z-10 w-80 rounded-xl border border-slate-200 bg-white p-3 text-xs leading-relaxed text-slate-700 shadow-lg opacity-0 transition-all group-hover:visible group-hover:opacity-100"
```

This is a tooltip - should use a dedicated `Tooltip` component, not inline styling.

---

## 5. ACCESSIBILITY ISSUES

### 5.1 Focus States

**Line 1354:**
```tsx
className="absolute right-4 top-4 rounded-full p-2 text-slate-400 transition hover:bg-slate-100 hover:text-slate-600"
```

**Problem:** Has `hover` state but no `focus-visible` state for keyboard navigation.

### 5.2 Color Contrast Issues

**Line 1318:**
```tsx
<ChevronDown className="h-3.5 w-3.5 text-slate-400" />
```

`text-slate-400` (#78716c or similar in light mode) on light backgrounds may have poor contrast.

**Line 1485:**
```tsx
<Info className="h-4 w-4 cursor-help text-slate-400 transition hover:text-slate-600" />
```

Same issue - slate-400 has insufficient contrast.

### 5.3 Hover-only Interactions

Multiple elements use `hover:` states but don't have mobile-friendly focus or active states:

```tsx
className="hover:bg-slate-100 hover:text-slate-600"  // Works on desktop, unusable on mobile
```

Should use:
```tsx
className="hover:bg-slate-100 focus:bg-slate-100 active:bg-slate-200 hover:text-slate-600 focus:text-slate-600"
```

### 5.4 Missing ARIA Attributes

**Line 1332-1344:** Modal overlay lacks proper ARIA attributes:
```tsx
<div className="fixed inset-0 z-50 flex items-center justify-center p-4">  // ❌ No role="dialog"
  <div className="absolute inset-0 h-full w-full bg-slate-950/70 backdrop-blur-sm" onClick={closeModal} />  // ❌ No aria-hidden
  <div className="relative z-10 w-full max-w-2xl rounded-3xl border border-slate-200 bg-white p-8 shadow-2xl">  // ❌ No role="document"
```

---

## 6. MAINTAINABILITY PROBLEMS

### 6.1 Duplicated Styling Code

The theme toggle button styling is verbose:

**Lines 30-52 in ThemeToggle.tsx:**
```tsx
className={`
  relative inline-flex h-8 w-16 items-center rounded-full transition-all duration-300
  border border-border bg-muted focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring
  hover:opacity-80 active:scale-95
`}
```

This works but should probably be its own component variant.

### 6.2 Magic Numbers Throughout

**Line 1277:** `pl-32 pr-32` - where does 32 come from? (2*16 = 32, assuming 16px base, but why?)

**Line 1840:** `h-[620px]` - arbitrary height for timeline

**Line 1832:** `rounded-3xl` - where does 1.5rem come from for this specific component?

**Line 292:** `const SHARE_QR_SIZE = 176; const FULLSCREEN_QR_SIZE = 768;` - magic numbers

### 6.3 No Semantic Component Abstractions

Multiple repeated patterns should be components:

1. **Status badges** (lines 1627-1633):
```tsx
<span className="inline-flex items-center gap-1 rounded-full border border-slate-200 px-3 py-1 text-[11px] font-medium text-slate-600">
  <div className={`h-2 w-2 rounded-full ${REPORT_STATUS_META[report.status].dot}`} />
  {REPORT_STATUS_META[report.status].label}
</span>
```

Should be: `<StatusBadge status={report.status} />`

2. **Info boxes** (lines 1584, 1843, 2238):
```tsx
<div className="rounded-2xl border border-amber-200 bg-amber-50 px-4 py-3 text-sm text-amber-700">
  {content}
</div>
```

Should be: `<InfoBox variant="warning">{content}</InfoBox>`

3. **Modal containers** (lines 1328-1344):
```tsx
<div className="fixed inset-0 z-50 flex items-center justify-center p-4">
  <div className="absolute inset-0 h-full w-full bg-slate-950/70 backdrop-blur-sm" onClick={closeModal} />
  <div className="relative z-10 w-full max-w-2xl rounded-3xl border border-slate-200 bg-white p-8 shadow-2xl">
    {/* content */}
  </div>
</div>
```

Should use a `<Modal>` component.

---

## 7. SPECIFIC FILE-BY-FILE ISSUES

### 7.1 `/Users/tkgshn/Developer/plural-reality/cartographer/app/components/ui/Button.tsx`

**Status:** GOOD - Uses design system colors correctly
- ✅ Uses `bg-primary`, `text-primary-foreground`, `bg-destructive`, etc.
- ✅ No hardcoded colors
- ✅ Proper focus and disabled states

**Minor issue - Line 27:**
```tsx
sm: "h-8 rounded-md px-3 text-xs",
```
Inconsistent: `rounded-md` for small button, but other buttons use `rounded-md` as default. Should define a `sm` size variant that uses consistent radius.

### 7.2 `/Users/tkgshn/Developer/plural-reality/cartographer/app/components/ui/card.tsx`

**Status:** GOOD - Uses design system correctly
- ✅ Uses `bg-card`, `border-border`, `text-card-foreground`
- ✅ Has consistent padding with `p-6`
- ✅ Proper shadow utilities

**Issue - Line 12:**
```tsx
className={cn(
  "rounded-lg border border-border bg-card text-card-foreground shadow-sm hover:shadow-md transition-shadow",
  className,
)}
```

The hover shadow might not work well in all contexts. Could be optional.

### 7.3 `/Users/tkgshn/Developer/plural-reality/cartographer/app/components/ui/input.tsx`

**Status:** MOSTLY GOOD but could be cleaner

**Line 14:** The full className is complex:
```tsx
"flex h-9 w-full rounded-md border border-input bg-background px-3 py-2 text-sm transition-colors file:border-0 file:bg-transparent file:text-sm file:font-medium file:text-foreground placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50"
```

This is correct (using design system) but very long. Could be broken into multiple lines.

### 7.4 `/Users/tkgshn/Developer/plural-reality/cartographer/app/components/ui/Skeleton.tsx`

**Status:** MIXED

**Good:**
- Uses `bg-muted` (design system)
- Has semantic variants

**Problems:**
- Line 25: `rounded` (no value) - should be `rounded-md` for consistency
- Line 35: `rounded-2xl` - overly large for skeleton

### 7.5 `/Users/tkgshn/Developer/plural-reality/cartographer/app/page.tsx`

**Status:** GOOD - Mostly follows design system

**No major issues**, but could optimize:
- Line 77: `text-muted-foreground` for loader is correct
- Line 108: Uses design system colors correctly

### 7.6 `/Users/tkgshn/Developer/plural-reality/cartographer/app/components/ThemeToggle.tsx`

**Status:** PROBLEMATIC

**Lines 30-34:**
```tsx
className={`
  relative inline-flex h-8 w-16 items-center rounded-full transition-all duration-300
  border border-border bg-muted focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring
  hover:opacity-80 active:scale-95
`}
```

Too many utilities. Should use a muted button variant.

**Line 43:**
```tsx
${theme === "dark" ? "translate-x-8" : "translate-x-0.5"}
```

Magic spacing values. Should define proper design tokens.

### 7.7 `/Users/tkgshn/Developer/plural-reality/cartographer/app/sessions/[sessionId]/[accessToken]/client-page.tsx`

**Status:** CRITICAL - 2,835 lines, massive styling issues

**Summary of issues in this file:**
1. Over 150 hardcoded color values
2. Inconsistent spacing (gap-1.5 vs gap-2 vs gap-2.5 vs gap-3, etc.)
3. Inconsistent border radius (rounded-lg vs rounded-xl vs rounded-2xl vs rounded-3xl)
4. Excessive complexity with 200+ character classNames
5. Repeated patterns throughout (should be extracted to components)
6. Magic numbers and arbitrary values
7. Dark mode handled inconsistently
8. Accessibility issues (focus states, color contrast)
9. Using specific component colors (pink, blue, purple) instead of semantic tokens

**Top 10 Problem Areas:**
1. **Lines 188-206:** REPORT_STATUS_META colors hardcoded
2. **Lines 221-264:** REPORT_TEMPLATES colors hardcoded
3. **Lines 1126, 1159, 1203, 1247, 1250:** Hardcoded text colors
4. **Lines 1277-1279:** Overly complex button className with magic padding
5. **Lines 1322-1344:** Modal styling all hardcoded
6. **Lines 1359-1470:** Report template selector with repeated patterns
7. **Lines 1510-1550:** Textarea and display divs with hardcoded colors/borders
8. **Lines 1584-1595:** Warning/info boxes with hardcoded colors
9. **Lines 1627-1633:** Status badges with inline color logic
10. **Lines 2054-2140:** Complex color switching based on status

---

## 8. DESIGN SYSTEM ANALYSIS

### 8.1 Current Design System (from `globals.css`)

**Light Mode:**
- `--background: 0 0% 100%` (white)
- `--foreground: 222.2 47.4% 11.2%` (dark slate)
- `--primary: 222.2 47.4% 11.2%` (same as foreground)
- `--secondary: 210 40% 96.1%` (light gray)
- `--card: 0 0% 100%` (white)
- `--border: 214.3 31.8% 85%` (light gray)
- `--ring: 222.2 84% 4.9%` (very dark, for focus)

**Dark Mode:**
- `--background: 0 0% 0%` (black)
- `--foreground: 210 40% 98%` (almost white)
- `--primary: 210 40% 98%` (light color)
- `--card: 0 0% 7%` (very dark gray)

### 8.2 Unused Design System Colors

The system defines these tokens but they're not always used:
- `card-foreground` (defined but rarely used)
- `accent` (defined but hardcoded colors used instead)
- `muted` (defined but slate-100/slate-800 used instead)
- `destructive` (defined but red-* colors hardcoded)

### 8.3 Missing Design System Tokens

The design system lacks:
- **Status colors:** warning, success, info, error (partially handled by destructive)
- **Semantic spacing:** gap-compact, gap-normal, gap-spacious
- **Semantic sizing:** size-sm, size-md, size-lg
- **Shadow system:** Only shadow-sm mentioned in implementation
- **Typography scale:** Font sizes not defined as tokens

---

## 9. RECOMMENDATIONS

### HIGH PRIORITY

1. **Eliminate all hardcoded Tailwind colors**
   - Replace `bg-slate-*`, `bg-red-*`, `text-blue-*` with design system colors
   - Create new CSS variables for status colors (warning, success, info)
   - Update REPORT_TEMPLATES and REPORT_STATUS_META to use semantic colors

2. **Extract repeated patterns to components:**
   - `<StatusBadge status={status} />`
   - `<AlertBox variant="warning|error|info" />`
   - `<Modal>`
   - `<TemplateCard>`
   - `<ParticipantProgressCard>`

3. **Define consistent spacing scale:**
   - Use only: `gap-2`, `gap-3`, `gap-4`, `gap-6` (skip 1.5, 2.5)
   - Document when to use which
   - Update `space-y-*` values consistently

4. **Standardize border radius:**
   - `rounded-md` for form elements
   - `rounded-lg` for cards
   - `rounded-xl` for popovers/tooltips
   - `rounded-full` for badges/buttons with circular intent

### MEDIUM PRIORITY

5. **Improve dark mode handling:**
   - Remove inline dark mode color specs
   - Ensure CSS variables handle all transitions
   - Test dark mode thoroughly

6. **Fix accessibility:**
   - Add `focus-visible` states to all interactive elements
   - Verify color contrast (WCAG AA minimum)
   - Add proper ARIA attributes to modals and complex widgets

7. **Extract magic numbers to constants:**
   - `const MODAL_MAX_WIDTH = "max-w-2xl"`
   - `const TIMELINE_HEIGHT = "h-[620px]"`
   - Create a `constants/sizes.ts`

8. **Break up `client-page.tsx`:**
   - Extract to smaller components
   - One file per feature section
   - Makes maintenance much easier

### LOW PRIORITY

9. **Create a component library:**
   - Document all component usage
   - Create a Storybook or similar
   - Make styling discoverable

10. **Add TypeScript props for styling:**
    - Create `variant` and `size` props instead of passing classNames
    - Use CVA (class-variance-authority) consistently across all components

---

## 10. EXAMPLE FIXES

### Fix 1: Replace hardcoded status colors

**Before:**
```tsx
const REPORT_STATUS_META: Record<SessionReportStatus, { label: string; dot: string; text: string }> = {
  pending: {
    label: "待機中",
    dot: "bg-amber-500 dark:bg-amber-400",
    text: "text-amber-700 dark:text-amber-400",
  },
  completed: {
    label: "完了",
    dot: "bg-emerald-500 dark:bg-emerald-400",
    text: "text-emerald-700 dark:text-emerald-400",
  },
};
```

**After:**
```tsx
// In globals.css - add:
:root {
  --status-pending: 45 93% 51%;  /* amber */
  --status-pending-foreground: 48 96% 53%;
  --status-completed: 142 71% 45%;  /* emerald */
  --status-completed-foreground: 142 72% 29%;
}

// In tailwind.config.ts:
colors: {
  status: {
    pending: "hsl(var(--status-pending))",
    'pending-foreground': "hsl(var(--status-pending-foreground))",
    completed: "hsl(var(--status-completed))",
    'completed-foreground': "hsl(var(--status-completed-foreground))",
  },
}

// In component:
const REPORT_STATUS_META: Record<SessionReportStatus, { label: string; dot: string; text: string }> = {
  pending: {
    label: "待機中",
    dot: "bg-status-pending",  // ✅ Uses design system
    text: "text-status-pending-foreground",
  },
  completed: {
    label: "完了",
    dot: "bg-status-completed",
    text: "text-status-completed-foreground",
  },
};
```

### Fix 2: Extract repeated TemplateCard pattern

**Before:**
```tsx
{[template1, template2, template3].map(template => (
  <button
    key={template.id}
    onClick={() => setSelectedTemplate(template.id)}
    className={`flex w-full items-start gap-4 rounded-2xl border p-4 text-left transition ${
      selectedTemplate === template.id
        ? "border-purple-400 bg-purple-50"
        : "border-slate-200 bg-white hover:border-slate-300 hover:bg-slate-50"
    }`}
  >
    <div className="rounded-lg bg-white p-2 shadow-sm">
      <template.icon className="h-5 w-5 text-slate-700" />
    </div>
    <div className="flex-1">
      <h3 className="text-base font-semibold text-slate-900 dark:text-slate-100">
        {template.name}
      </h3>
      <p className="mt-1 text-sm text-slate-700 dark:text-slate-300">
        {template.description}
      </p>
    </div>
  </button>
))}
```

**After:**
```tsx
// New component: TemplateCard.tsx
import { cn } from "@/lib/utils";

interface TemplateCardProps {
  template: ReportTemplate;
  isSelected: boolean;
  onClick: () => void;
}

export function TemplateCard({ template, isSelected, onClick }: TemplateCardProps) {
  return (
    <button
      onClick={onClick}
      className={cn(
        "flex w-full items-start gap-4 rounded-lg border p-4 text-left transition",
        isSelected
          ? "border-primary bg-primary/10"
          : "border-border bg-card hover:bg-muted"
      )}
    >
      <div className="rounded-md bg-background p-2 shadow-sm">
        <template.icon className="h-5 w-5 text-primary" />
      </div>
      <div className="flex-1">
        <h3 className="text-base font-semibold text-foreground">
          {template.name}
        </h3>
        <p className="mt-1 text-sm text-muted-foreground">
          {template.description}
        </p>
      </div>
    </button>
  );
}

// In client-page.tsx:
{reportTemplates.map(template => (
  <TemplateCard
    key={template.id}
    template={template}
    isSelected={selectedTemplate === template.id}
    onClick={() => setSelectedTemplate(template.id)}
  />
))}
```

### Fix 3: Use proper component for modal

**Before:**
```tsx
<div className="fixed inset-0 z-50 flex items-center justify-center p-4">
  <div className="absolute inset-0 h-full w-full bg-slate-950/70 backdrop-blur-sm" onClick={closeModal} />
  <div className="relative z-10 w-full max-w-2xl rounded-3xl border border-slate-200 bg-white p-8 shadow-2xl">
    {/* content */}
  </div>
</div>
```

**After:**
```tsx
// Use or create a Modal component
<Modal isOpen={showReportModal} onClose={() => setShowReportModal(false)}>
  <ModalHeader>
    <h2>レポート生成設定</h2>
  </ModalHeader>
  <ModalContent>
    {/* content */}
  </ModalContent>
</Modal>
```

---

## SUMMARY TABLE

| Category | Severity | Count | Example |
|----------|----------|-------|---------|
| Hardcoded Colors | CRITICAL | 150+ | `text-slate-900 dark:text-slate-100` |
| Inconsistent Spacing | HIGH | 30+ | `gap-1.5 vs gap-2 vs gap-2.5 vs gap-3` |
| Inconsistent Radius | HIGH | 15+ | `rounded-lg vs rounded-xl vs rounded-2xl` |
| Magic Numbers | MEDIUM | 20+ | `pl-32 pr-32`, `h-[620px]` |
| Long Classnames | MEDIUM | 40+ | 200+ character className strings |
| Repeated Patterns | MEDIUM | 20+ | Status badges, alert boxes, cards |
| Missing A11y | MEDIUM | 10+ | No focus states, color contrast issues |
| Missing Components | LOW | 15+ | Modals, alerts, status badges as inline divs |

---

## CONCLUSION

The Cartographer frontend has a well-designed system foundation but poor execution. **The design system is present but ignored**. Fixing this requires:

1. **Immediate:** Replace hardcoded colors with design system tokens
2. **Short-term:** Extract repeated patterns to components
3. **Medium-term:** Standardize spacing and sizing scales
4. **Long-term:** Build a comprehensive component library

The most impactful change would be refactoring `client-page.tsx` into smaller, focused components. This 2,800+ line file is unmaintainable and should be broken into at least 8-10 smaller files.

