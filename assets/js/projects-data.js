/* ==========================================================================
   PROJECTS DATA
   Edit via the admin tool, or by hand — this is the single source of truth
   for both the homepage cards and the shared case-study template.
   ========================================================================== */

const GITHUB_REPO = "Abdullah-5121/Abdullah-5121.github.io";
const GITHUB_BRANCH = "main";

function nbviewerLink(path){
  return `https://nbviewer.org/github/${GITHUB_REPO}/blob/${GITHUB_BRANCH}/${path}`;
}

const PROJECTS = [
  {
    id: "cyclistic",
    category: "analytics",
    title: "Cyclistic Bike-Share Analysis",
    blurb: "Compared casual riders vs. annual members across a full year of ride data to shape a membership-conversion strategy.",
    subtitle: "Twelve months of ride data, one question: what actually separates a casual rider from an annual member — and how do you turn one into the other?",
    image: "assets/img/portfolio/casestudy1.jpg",
    gallery: [
      "assets/img/portfolio/casestudy1.jpg",
      "assets/img/00_cyclistic/01_Bar.PNG",
      "assets/img/00_cyclistic/02_Bar.PNG",
      "assets/img/00_cyclistic/03_How.PNG",
      "assets/img/00_cyclistic/04_Rec.PNG",
      "assets/img/00_cyclistic/05_DB.PNG"
    ],
    stack: [
      "R",
      "dplyr",
      "lubridate",
      "Tableau",
      "PowerPoint"
    ],
    out: "Out[]: usage patterns → 3 targeted conversion recommendations",
    objective: "Converting casual riders into annual members — a full analysis of 12 months of Cyclistic bike-share data to identify the behavioral differences that a marketing strategy could act on.",
    steps: [
      {
        t: "Data cleaning & wrangling",
        d: "consolidated and pre-processed 12 months of ride logs in R and RStudio."
      },
      {
        t: "Statistical summary",
        d: "calculated average ride length and total ride counts by rider type."
      },
      {
        t: "Visualization",
        d: "built Tableau dashboards showing usage patterns by day and month."
      }
    ],
    finding: "Casual riders take significantly longer rides on weekends for leisure, while annual members take shorter, frequent commute rides on weekdays.",
    recommendation: "Launch a weekend-focused promotional campaign — a \"Weekend Warrior Annual Pass\" — that speaks directly to casual riders' leisure usage pattern.",
    dataSource: "Cyclistic ride data — <a class=\"text-accent\" href=\"https://docs.google.com/spreadsheets/d/1uCTsHlZLm4L7-ueaSLwDg0ut3BP_V4mKDo2IMpaXrk4/template/preview?resourcekey=0-dQAUjAu2UUCsLEQQt20PDA#gid=1797029090\" target=\"_blank\" rel=\"noopener noreferrer\">2019</a> &amp; <a class=\"text-accent\" href=\"https://docs.google.com/spreadsheets/d/179QVLO_yu5BJEKFVZShsKag74ZaUYIF6FevLYzs3hRc/template/preview#gid=640449855\" target=\"_blank\" rel=\"noopener noreferrer\">2020</a>",
    links: {
      rScript: "01_Cyclistic_Rides/01_R-Work/cyclistic_analysis.R",
      dashboard: "https://public.tableau.com/app/profile/muhammad.abdullah6976/viz/CyclisticDataset_17539250254980/Dashboard1",
      presentation: "01_Cyclistic_Rides/02_Presentation/01_Casestudy-1_Report.pdf",
      dataset: "01_Cyclistic_Rides/summary(Final).csv",
      github: "https://github.com/Abdullah-5121/Abdullah-5121.github.io/tree/48be586841b70dace5c03b67e424f17bd464a9e4/01_Cyclistic_Rides"
    }
  },
  {
    id: "superstore",
    category: "analytics",
    title: "Superstore Sales Performance",
    blurb: "Diagnosed profitability leaks across regions, categories and shipping modes for a national retail chain.",
    subtitle: "A national retail chain's full order history, broken down by region, category and shipping mode to find exactly where profit was leaking.",
    image: "assets/img/portfolio/casestudy2.jpg",
    gallery: [
      "assets/img/portfolio/casestudy2.jpg",
      "assets/img/01_superstore/01_BAR.PNG",
      "assets/img/01_superstore/02_BAR.PNG",
      "assets/img/01_superstore/03_BAR.PNG",
      "assets/img/01_superstore/04_BAR.PNG",
      "assets/img/01_superstore/05_Rec.PNG",
      "assets/img/01_superstore/06_Dashboard.PNG"
    ],
    stack: [
      "R",
      "ggplot2",
      "Tableau",
      "Excel"
    ],
    out: "Out[]: 3 underperforming segments flagged for discount-policy review",
    objective: "Diagnosing where a national retail chain's profitability was breaking down — across regions, product categories, and shipping modes — to guide a discount and shipping-policy review.",
    steps: [
      {
        t: "Data cleaning",
        d: "standardized and de-duplicated the full order-level dataset in R."
      },
      {
        t: "Segmentation",
        d: "broke down sales & profit by region, category, sub-category and shipping mode."
      },
      {
        t: "Visualization",
        d: "built an interactive Tableau dashboard for stakeholders to drill into any segment."
      }
    ],
    finding: "A small set of sub-categories and regions were consistently sold at a loss, largely driven by aggressive discounting on already thin-margin items.",
    recommendation: "Cap or tier discounts on the flagged sub-categories, and re-evaluate shipping cost allocation for the lowest-margin regions.",
    dataSource: "Superstore dataset — <a class=\"text-accent\" href=\"https://www.kaggle.com/datasets/vivek468/superstore-dataset-final\" target=\"_blank\" rel=\"noopener noreferrer\">Kaggle</a>",
    links: {
      rScript: "02_SuperStore/01_R Work/SuperStore Dataset ( Cleaning + Analysis + Visualization).R",
      kaggleDataset: "https://www.kaggle.com/datasets/vivek468/superstore-dataset-final",
      dashboard: "https://public.tableau.com/app/profile/muhammad.abdullah6976/viz/SuperStoreDatasetVisualization/Dashboard1",
      presentation: "02_SuperStore/02_Presentation/SuperStore Presentation.pdf",
      dataset: "02_SuperStore/Cleaned_Superstore.csv",
      github: "https://github.com/Abdullah-5121/Abdullah-5121.github.io/tree/0a22f15043b0ea1d29acde2b1068e5c6e0b9740f/02_SuperStore"
    }
  },
  {
    id: "fitbit",
    category: "analytics",
    title: "Fitbit User Behaviour Analysis",
    blurb: "Explored wearable-device data to classify users by activity level and surface habits linked to higher calorie burn.",
    subtitle: "Wearable-device data from real users, explored to classify activity levels and surface the habits linked to higher calorie burn.",
    image: "assets/img/portfolio/casestudy3.jpg",
    gallery: [
      "assets/img/portfolio/casestudy3.jpg",
      "assets/img/02_fitbit/01_PIE.PNG",
      "assets/img/02_fitbit/02_Bar.PNG",
      "assets/img/02_fitbit/03_sctr.PNG",
      "assets/img/02_fitbit/04_Sctr.PNG",
      "assets/img/02_fitbit/05_Recommend.PNG",
      "assets/img/02_fitbit/06_DB.PNG"
    ],
    stack: [
      "R",
      "EDA",
      "Kaggle",
      "Tableau"
    ],
    out: "Out[]: 4 user activity segments identified for a wellness app",
    objective: "Exploring wearable fitness-tracker data to classify users by activity level and identify the daily habits most strongly linked to higher calorie burn — useful signal for a wellness app's engagement strategy.",
    steps: [
      {
        t: "Data cleaning",
        d: "merged and cleaned daily activity logs across the tracked user panel."
      },
      {
        t: "Exploratory analysis",
        d: "correlated steps, active minutes and distance against calories burned."
      },
      {
        t: "Segmentation",
        d: "classified users into activity tiers based on average daily steps."
      },
      {
        t: "Publishing",
        d: "shipped the full exploratory notebook and write-up on Kaggle."
      }
    ],
    finding: "Users fell into four clear activity tiers, and \"very active minutes\" tracked much more tightly with calorie burn than total step count alone.",
    recommendation: "Design engagement nudges around active-minute streaks rather than raw step goals, especially for users in the lower activity tiers.",
    dataSource: "Fitbit Fitness Tracker — <a class=\"text-accent\" href=\"https://www.kaggle.com/datasets/arashnic/fitbit\" target=\"_blank\" rel=\"noopener noreferrer\">Kaggle</a>",
    links: {
      rScript: "03_Fitbit_Consumers/02_R_Work/02_Fitbit(Cleaning+Analysis).R",
      kaggleNotebook: "https://www.kaggle.com/code/mabdullah5121/fitbit-user-behaviour-analysis",
      kaggleDataset: "https://www.kaggle.com/datasets/arashnic/fitbit",
      dashboard: "https://public.tableau.com/app/profile/muhammad.abdullah6976/viz/FitbeatsDataset/Dashboard3#1",
      presentation: "03_Fitbit_Consumers/03_Presentation/Final_Presentation_Fitbits_Dataset.pdf",
      dataset: "03_Fitbit_Consumers/01_daily_activity_cleaned.csv",
      github: "https://github.com/Abdullah-5121/Abdullah-5121.github.io/tree/52605f03017504d97ab0285317ce520f8e634eb2/03_Fitbit_Consumers"
    }
  },
  {
    id: "pubg",
    category: "analytics",
    title: "PUBG Player Performance Analysis",
    blurb: "Mined 6.4M rows of match data to uncover the habits that separate top-percentile players from the rest.",
    subtitle: "6.4 million rows of match data, mined to find out exactly what separates top-decile players from everyone else.",
    image: "assets/img/portfolio/casestudy4.jpg",
    gallery: [
      "assets/img/portfolio/casestudy4.jpg",
      "assets/img/03_pubg/01_Pie.PNG",
      "assets/img/03_pubg/02_bar1.PNG",
      "assets/img/03_pubg/03_Scatter_plt.PNG",
      "assets/img/03_pubg/04_Box.PNG",
      "assets/img/03_pubg/05_Recommend.PNG",
      "assets/img/03_pubg/06_Dash.PNG"
    ],
    stack: [
      "R",
      "Large-scale EDA",
      "Kaggle",
      "Tableau"
    ],
    out: "Out[]: 6.4M rows processed → top-decile player profile",
    objective: "Processing 6.4 million rows of PUBG match data to build a statistical profile of top-decile players — the habits and stats that most reliably separate them from the rest of the player base.",
    steps: [
      {
        t: "Large-scale cleaning",
        d: "filtered, cleaned and sampled 6.4M rows of match records down to an analysis-ready set."
      },
      {
        t: "Statistical profiling",
        d: "compared kills, damage, walk distance and survival time across performance tiers."
      },
      {
        t: "Visualization",
        d: "built a Tableau dashboard to compare tiers across match types."
      },
      {
        t: "Publishing",
        d: "shipped the full notebook and write-up on Kaggle."
      }
    ],
    finding: "Top-decile players didn't just get more kills — they covered significantly more ground per match, pointing to positioning and rotation as a bigger differentiator than raw aim.",
    recommendation: "Frame skill-improvement content (or in-game coaching tips) around map movement and rotation timing, not just combat mechanics.",
    dataSource: "PUBG match dataset — <a class=\"text-accent\" href=\"https://www.kaggle.com/datasets/deepanjhandas/pubg-game-dataset\" target=\"_blank\" rel=\"noopener noreferrer\">Kaggle</a>",
    links: {
      rScript: "04_Pubg/02_R_Work/01_PUBG(Cleaning+Analysis).R",
      kaggleNotebook: "https://www.kaggle.com/code/mabdullah5121/pubg-players-performance-analysis",
      kaggleDataset: "https://www.kaggle.com/datasets/deepanjhandas/pubg-game-dataset",
      dashboard: "https://public.tableau.com/app/profile/muhammad.abdullah6976/viz/Pubg_Dataset/Final_Dashboard",
      presentation: "04_Pubg/03_Presentation/PUBG_DATASET_PRESENTATION.pdf",
      dataset: "04_Pubg/cleaned_pubg_dataset_Tableau.csv",
      github: "https://github.com/Abdullah-5121/Abdullah-5121.github.io/tree/0719a253d23a41876b6e1da401415963671a6661/04_Pubg"
    }
  },
  {
    id: "Salifort Motors",
    category: "data-science",
    title: "Salifort Motors — Employee Churn Prediction & Retention Strategy",
    blurb: "",
    subtitle: "Building an end-to-end Machine Learning pipeline in Python to predict employee turnover, identify key risk drivers, and deliver data-driven retention strategies for executive leadership.",
    image: "assets/img/portfolio/casestudy5.png",
    gallery: [
      "assets/img/portfolio/casestudy5.png",
      "assets/img/04_Salifort_Motors/01.png",
      "assets/img/04_Salifort_Motors/02.png",
      "assets/img/04_Salifort_Motors/03.png",
      "assets/img/04_Salifort_Motors/04.png",
      "assets/img/04_Salifort_Motors/05.png",
      "assets/img/04_Salifort_Motors/06.png"
    ],
    stack: [
      "Python",
      "Scikit-Learn",
      "xgboost",
      "Large-scale EDA",
      "Tableau"
    ],
    out: "",
    objective: "To perform exploratory data analysis to uncover the root causes of employee overwork and turnover, train and tune tree-based machine learning models (Random Forest, XGBoost) to accurately predict at-risk staff before they resign, and translate model feature importances into actionable HR policies—like workload caps and career pathways—to increase retention and reduce hiring costs.",
    steps: [
      {
        t: "Data cleaning & preprocessing",
        d: "handled missing values, encoded categorical features, and structured the dataset for tree-based architectures."
      },
      {
        t: "Exploratory data analysis",
        d: "isolated overwork clusters, pay stagnation, and project underutilization driving high-risk employee departures."
      },
      {
        t: "Predictive modeling",
        d: "trained, hyperparameter-tuned, and evaluated Decision Tree, Random Forest, and XGBoost classifiers via grid search."
      },
      {
        t: "Evaluation & deployment",
        d: "achieved a 99% test F1-score with Random Forest, extracted Gini feature importances, and shipped actionable HR retention policies."
      }
    ],
    finding: "Overwork (>250 monthly hours across 6+ projects) and career stagnation (low salary tiers with zero promotions in 5 years) drove the highest rates of employee churn. Mid-tenure employees between 3 to 5 years were the most vulnerable to voluntary departure",
    recommendation: "Cap monthly work hours at 200, restrict project assignments to 3–5 per employee, and establish clear 3-year career promotion pathways. Re-evaluate compensation for stagnated low-salary roles and integrate the Random Forest model into HR workflows for proactive quarterly stay interviews",
    notebook: "05_Salifort_Motors/00_salifort_motors_py.ipynb",
    links: {
      kaggleNotebook: "https://www.kaggle.com/code/mabdullah5121/salifort-employee-churn-prediction",
      dashboard: "https://public.tableau.com/app/profile/muhammad.abdullah6976/viz/EmplyeeChurnAnalysis/FinalDashboard",
      presentation: "05_Salifort_Motors/03_Presentation/00_Salifort Motors HR Analytics - Employee Churn _ Retention Strategy.pdf",
      dataset: "05_Salifort_Motors/01_Dataset/01_HR_capstone_dataset.csv",
      github: "wait for it"
    }
  }
];
