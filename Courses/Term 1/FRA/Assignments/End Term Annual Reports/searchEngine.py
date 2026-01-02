import re
import pdfplumber

# Extract text from PDF
pdf_path = "Annual-Report-2021-2022 Pidilite.pdf"
text = ""
with pdfplumber.open(pdf_path) as pdf:
    for page in pdf.pages:
        text += page.extract_text() + "\n"

# Regex patterns
balance_sheet = re.search(r"BALANCE SHEET[\s\S]*?(?=STATEMENT OF PROFIT AND LOSS)", text)
income_statement = re.search(r"STATEMENT OF PROFIT AND LOSS[\s\S]*?(?=In terms of our report attached)", text)

# Save / print results
if balance_sheet:
    with open("Balance_Sheet.txt", "w", encoding="utf-8") as f:
        f.write(balance_sheet.group(0))
    print("✅ Balance Sheet extracted.")

if income_statement:
    with open("Income_Statement.txt", "w", encoding="utf-8") as f:
        f.write(income_statement.group(0))
    print("✅ Income Statement extracted.")
