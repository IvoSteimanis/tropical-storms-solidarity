"""
Post-process esttab .tex tables for LaTeX/Quarto compatibility.
Run after the Stata pipeline: python scripts/clean_tex_tables.py

Fixes:
  1. Strip outer {..} group wrapper and \def\sym (moved to preamble)
  2. Replace Unicode delta with LaTeX equivalent
  3. Escape # in Stata interaction notation (c.X#c.Y)
  4. Replace unescaped & in text labels (not column separators)
  5. Copy cleaned files to submission/tables/ for Quarto rendering
"""
import os, re, glob, shutil

SRC_DIR = os.path.join(os.path.dirname(__file__), "..", "results", "tables")
DST_DIR = os.path.normpath(os.path.join(os.path.dirname(__file__),
    "..", "..", "submission", "2026 - NCC v4", "tables"))

os.makedirs(DST_DIR, exist_ok=True)

REPLACEMENTS = [
    ("∆", r"$\Delta$"),
    ("²", r"$^{2}$"),
]

def clean_tex(content):
    lines = content.split("\n")

    # Strip outer { wrapper and \def\sym line
    if lines and lines[0].strip() == "{":
        lines = lines[1:]
    if lines and lines[0].strip().startswith(r"\def\sym"):
        lines = lines[1:]
    while lines and lines[-1].strip() == "":
        lines = lines[:-1]
    if lines and lines[-1].strip() == "}":
        lines = lines[:-1]

    content = "\n".join(lines)

    # Unicode replacements
    for old, new in REPLACEMENTS:
        content = content.replace(old, new)

    # Escape # in Stata interaction terms (c.X#c.Y) but NOT in \def\sym#1
    content = re.sub(r'(?<!\\sym)#(?!1)', r'\\#', content)

    # Fix unescaped & in label text (F-Test lines where & is not a column sep)
    content = re.sub(
        r'(F-Test:[^&\n]*?) & ([^&\n]*?(?:&|\\\\\s*$))',
        lambda m: m.group(0).replace(' & ', r' \& ', 1)
            if m.group(0).count('&') > 1 else m.group(0),
        content, flags=re.MULTILINE
    )

    # Remove \multicolumn notes lines (between \bottomrule and \end{tabular})
    # These cause rendering issues inside \scriptsize floats; notes are in the .qmd instead
    # Match entire line containing \multicolumn + \footnotesize through to the line-ending \\
    content = re.sub(
        r'^\\multicolumn\{.*\\footnotesize.*\\\\$',
        '', content, flags=re.MULTILINE)

    return content


count = 0
for f in sorted(glob.glob(os.path.join(SRC_DIR, "*.tex"))):
    with open(f, "r", encoding="utf-8") as fh:
        content = fh.read()
    cleaned = clean_tex(content)
    outpath = os.path.join(DST_DIR, os.path.basename(f))
    with open(outpath, "w", encoding="utf-8", newline="\n") as fh:
        fh.write(cleaned)
    changes = content != cleaned
    if changes:
        count += 1
        print(f"  cleaned: {os.path.basename(f)}")
    else:
        print(f"  copied:  {os.path.basename(f)}")

print(f"\n{count} files modified, {len(glob.glob(os.path.join(DST_DIR, '*.tex')))} total in {DST_DIR}")
