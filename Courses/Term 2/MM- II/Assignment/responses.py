import pandas as pd

# ========= 1. FILE PATHS =========
INPUT_FILE = r"MM2_150_responses_Q1_Q25.xlsx"   # <- update if needed
OUTPUT_FILE = r"MM2_150_responses_Q1_Q25_numeric_START1.xlsx"

# ========= 2. LOAD DATA =========
df = pd.read_excel(INPUT_FILE)

# ========= 3. MAPPINGS (ALL START FROM 1) =========

# Q7: Order frequency
q7_map = {
    "Never": 1,
    "Rarely": 2,
    "Monthly": 3,
    "Twice a month": 4,
    "Weekly": 5,
    "2–3 times/week": 6,
    "Daily": 7,
}

# Q8: Received poor quality perishable
q8_map = {
    "No": 1,
    "Yes": 2,
}

# Q10: Poor quality frequency
q10_map = {
    "0 times": 1,
    "1 time": 2,
    "2–3 times": 3,
    "4–5 times": 4,
    "6+ times": 5,
}

# Q12: Used Zepto
q12_map = {
    "No": 1,
    "Yes": 2,
}

# Q18: Likely to continue next month
q18_map = {
    "Very unlikely": 1,
    "Unlikely": 2,
    "Neutral": 3,
    "Likely": 4,
    "Very likely": 5,
}

# Q20: Recommend Zepto
q20_map = {
    "Definitely no": 1,
    "Probably no": 2,
    "Might or might not": 3,
    "Probably yes": 4,
    "Definitely yes": 5,
}

# Q23: Likely to subscribe
q23_map = {
    "Very unlikely": 1,
    "Unlikely": 2,
    "Neutral": 3,
    "Likely": 4,
    "Very likely": 5,
}

# Q25: Premium % willing
q25_map = {
    "0%": 1,
    "up to 5%": 2,
    "6–10%": 3,
    "11–20%": 4,
    ">20%": 5,
}

# ========= 4. APPLY MAPPINGS =========
df["Q7_num"]  = df["Q7_Order_frequency_Q_commerce"].map(q7_map)
df["Q8_num"]  = df["Q8_Received_poor_quality_perishable"].map(q8_map)
df["Q10_num"] = df["Q10_Poor_quality_frequency_6_months"].map(q10_map)
df["Q12_num"] = df["Q12_Used_Zepto"].map(q12_map)
df["Q18_num"] = df["Q18_Likely_continue_next_month"].map(q18_map)
df["Q20_num"] = df["Q20_Recommend_Zepto"].map(q20_map)
df["Q23_num"] = df["Q23_Subscription_likely_to_subscribe"].map(q23_map)
df["Q25_num"] = df["Q25_Premium_percent_willing"].map(q25_map)

# ========= 5. Q11 MULTI-RESPONSE → DUMMIES (START FROM 1/2) =========
q11_col = "Q11_Reaction_to_poor_quality"

actions = [
    "Complain to customer support",
    "Return item in app",
    "Post on social media",
    "Complain to friends/family",
    "Do nothing",
]

for act in actions:
    col_name = "Q11_" + act.replace(" ", "_").replace("/", "_")
    df[col_name] = df[q11_col].fillna("").apply(
        lambda x: 2 if act in str(x) else 1   # 1 = not selected, 2 = selected
    )

# Total number of actions taken (now starts from 1 instead of 0)
df["Q11_num_actions"] = df[[ 
    "Q11_" + a.replace(" ", "_").replace("/", "_") for a in actions 
]].sum(axis=1)

# ========= 6. SAVE FILE =========
df.to_excel(OUTPUT_FILE, index=False)
print(f"✅ File saved successfully as: {OUTPUT_FILE}")
