/* ==========================================================================
   SKILLS DATA
   Edit via the admin tool, or by hand — powers the animated skill bars on
   the homepage and the "Tools & Proficiency" bars on case-study pages.
   ========================================================================== */

const SKILL_CATEGORIES = [
  {
    icon: "bi-braces-asterisk",
    title: "Languages & Querying",
    desc: "For wrangling and interrogating data directly.",
    skills: [
      {
        name: "SQL (Oracle)",
        percent: 90
      },
      {
        name: "Python",
        percent: 95
      },
      {
        name: "C++",
        percent: 90
      },
      {
        name: "R",
        percent: 80
      }
    ]
  },
  {
    icon: "bi-bar-chart-line",
    title: "Analysis & Modeling",
    desc: "Making sense of the numbers, statistically.",
    skills: [
      {
        name: "Pandas",
        percent: 85
      },
      {
        name: "Exploratory Data Analysis",
        percent: 88
      },
      {
        name: "Regression",
        percent: 85
      },
      {
        name: "Classification",
        percent: 90
      }
    ]
  },
  {
    icon: "bi-easel2",
    title: "Visualization & BI",
    desc: "Turning findings into something a stakeholder can read in a glance.",
    skills: [
      {
        name: "Tableau",
        percent: 92
      },
      {
        name: "Matplotlib",
        percent: 90
      },
      {
        name: "Seaborn",
        percent: 85
      },
      {
        name: "PowerPoint",
        percent: 84
      }
    ]
  },
  {
    icon: "bi-diagram-3",
    title: "CS Foundations",
    desc: "The systems layer underneath the analysis.",
    skills: [
      {
        name: "Data Structures & Algorithms",
        percent: 90
      },
      {
        name: "OOPs",
        percent: 90
      },
      {
        name: "Probaility & Stats",
        percent: 85
      },
      {
        name: "HCI",
        percent: 90
      }
    ]
  },
  {
    icon: "bi-kanban",
    title: "Workflow & Delivery",
    desc: "How the work gets built, tracked and shipped.",
    skills: [
      {
        name: "Git & GitHub",
        percent: 85
      },
      {
        name: "Jupyter",
        percent: 80
      },
      {
        name: "Kaggle",
        percent: 82
      },
      {
        name: "Client Delivery (Fiverr)",
        percent: 88
      }
    ]
  },
  {
    icon: "bi-patch-check",
    title: "Certified",
    desc: "Formal training backing the practical work.",
    skills: [
      {
        name: "Google Data Analytics Professional Certificate"
      },
      {
        name: "Google Advanced Data Analytics Professional Certificate"
      }
    ],
    badge: true
  }
];

const SKILL_PERCENT = {};
SKILL_CATEGORIES.forEach(cat => {
  if (cat.badge) return;
  cat.skills.forEach(s => { SKILL_PERCENT[s.name.toLowerCase()] = s.percent; });
});
SKILL_PERCENT["large-scale eda"] = SKILL_PERCENT["exploratory data analysis"];
SKILL_PERCENT["eda"] = SKILL_PERCENT["exploratory data analysis"];
SKILL_PERCENT["lubridate"] = SKILL_PERCENT["r"];
SKILL_PERCENT["dplyr"] = SKILL_PERCENT["r"];

function skillPercentFor(name){
  return SKILL_PERCENT[(name || "").toLowerCase()] || 85;
}
