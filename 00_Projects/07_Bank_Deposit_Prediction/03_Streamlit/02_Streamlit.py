import streamlit as st
import pandas as pd
import joblib
from pathlib import Path

BASE_DIR = Path(__file__).resolve().parent

pipeline = joblib.load(BASE_DIR / '01_Model' / 'bank_deposit_pipeline.pkl')
st.set_page_config(page_title="Bank Term Deposit Predictor", page_icon="🏦")
st.title("🏦 Bank Term Deposit Subscription Predictor")
st.write("Predict whether a client will subscribe to a term deposit.")

col1, col2 = st.columns(2)

with col1:
    age = st.number_input("Age", min_value=18, max_value=100, value=35)
    job = st.selectbox("Job", ['management','technician','entrepreneur','blue-collar',
                                'unknown','retired','admin.','services','self-employed',
                                'unemployed','housemaid','student'])
    marital = st.selectbox("Marital Status", ['married', 'single', 'divorced'])
    education = st.selectbox("Education", ['primary', 'secondary', 'tertiary', 'unknown'])
    balance = st.number_input("Account Balance (€)", value=1000)
    housing = st.selectbox("Housing Loan?", ['yes', 'no'])
    loan = st.selectbox("Personal Loan?", ['yes', 'no'])
    contact = st.selectbox("Contact Type", ['cellular', 'telephone', 'unknown'])

with col2:
    day = st.number_input("Last Contact Day", min_value=1, max_value=31, value=15)
    month = st.selectbox("Last Contact Month", ['jan','feb','mar','apr','may','jun',
                                                  'jul','aug','sep','oct','nov','dec'])
    duration = st.number_input("Call Duration (seconds)", min_value=0, value=180)
    campaign = st.number_input("Contacts This Campaign", min_value=1, value=1)
    pdays = st.number_input("Days Since Last Contact (-1 = never)", value=-1)
    previous = st.number_input("Previous Contacts", min_value=0, value=0)
    poutcome = st.selectbox("Previous Outcome", ['success', 'failure', 'other', 'unknown'])

if st.button("Predict"):
    input_df = pd.DataFrame([{
        'age': age, 'job': job, 'marital': marital, 'education': education,
        'balance': balance, 'housing': housing, 'loan': loan, 'contact': contact,
        'day': day, 'month': month, 'duration': duration, 'campaign': campaign,
        'pdays': pdays, 'previous': previous, 'poutcome': poutcome
    }])

    prediction = pipeline.predict(input_df)[0]
    probability = pipeline.predict_proba(input_df)[0][1]

    if prediction == 1:
        st.success(f"✅ Likely to Subscribe — Probability: {probability:.1%}")
    else:
        st.error(f"❌ Unlikely to Subscribe — Probability: {probability:.1%}")