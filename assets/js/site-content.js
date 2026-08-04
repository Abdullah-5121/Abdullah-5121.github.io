/* ==========================================================================
   SITE CONTENT
   Edit via the admin tool, or by hand — hero, about, resume, contact and
   footer copy for the homepage.
   ========================================================================== */

const SITE_CONTENT = {
  hero: {
    kicker: "CHAKWAL, PAKISTAN — AVAILABLE FOR FREELANCE",
    firstName: "Muhammad",
    lastName: "Abdullah",
    roles: [
      "Data Analyst",
      "Data Scientist"
    ],
    description: "I turn messy, real-world datasets into dashboards, models and stories that hold up under questions — using R, SQL, Python and Tableau, with a Google Advanced Data Analytics certification behind the workflow.",
    stats: [
      {
        value: 4,
        suffix: "+",
        label: "Case Studies"
      },
      {
        value: 6.4,
        suffix: "M",
        label: "Rows Analyzed (PUBG)"
      },
      {
        value: 3.78,
        suffix: "",
        label: "CS GPA"
      }
    ]
  },
  about: {
    photo: "assets/img/my-profile-img.jpeg",
    badge: "status: open_to_work",
    paragraphs: [
      "I am a Computer Science undergraduate (5th Semester) at the University of Chakwal and a freelance Data Scientist with proven expertise in building end-to-end analytical solutions. Holding both the Google Data Analytics and Google Advanced Data Analytics Professional Certificates, I specialize in transforming raw, multi-dimensional datasets into high-impact predictive models, executive dashboards, and actionable business strategy.",
      "My workflow spans the entire data science lifecycle—from deep exploratory data analysis (EDA), data cleaning, and feature engineering to training predictive ML architectures (Decision Trees, Random Forest, XGBoost) and designing dynamic Tableau dashboards."
    ],
    facts: [
      {
        label: "Location",
        value: "Chakwal, Pakistan"
      },
      {
        label: "Degree",
        value: "BS Computer Science"
      },
      {
        label: "GPA",
        value: "3.78"
      },
      {
        label: "Freelance",
        value: "Data Science · Fiverr"
      },
      {
        label: "Certified",
        value: "Google Advanced Data Analytics"
      },
      {
        label: "Email",
        value: "mabdullahpro437@gmail.com"
      }
    ]
  },
  sectionIntros: {
    about: {
      cellNum: 2,
      cellText: "about.summary()",
      title: "About Me",
      desc: "The short version of how I work — and the facts that don't fit in a headline."
    },
    skills: {
      cellNum: 3,
      cellText: "skills.list_all()",
      title: "Skills & Tools",
      desc: "What I reach for, grouped by the job it does — from raw query to finished story."
    },
    projects: {
      cellNum: 4,
      cellText: 'projects.render(filter=<span class="text-amber">"all"</span>)' ,
      title: "Projects",
      desc: "Split into two shelves: dashboard-driven analytics case studies, and code-first data science notebooks published on Kaggle."
    },
    resume: {
      cellNum: 5,
      cellText: "resume.timeline()",
      title: "Resume",
      desc: "Education and hands-on experience, in order."
    },
    contact: {
      cellNum: 6,
      cellText: "contact.send(message)",
      title: "Let's Work Together",
      desc: "Open to freelance data analysis / data science work, collaborations, and interesting datasets."
    }
  },
  resume: {
    education: [
      {
        when: "In Progress",
        title: "BS Computer Science (5th Semester)",
        org: "University of Chakwal · CGPA 3.71",
        bullets: [
          "Coursework spanning C++ data structures & algorithms, object-oriented design, computer networks, MIPS assembly, and database management systems (SQL)",
          "Directly applying core CS principles to engineer end-to-end Python, SQL, and machine learning pipelines",
          "Engineering efficient data processing pipelines and structured software architectures for complex data science problems"
        ]
      },
      {
        when: "Certifications",
        title: "Google Data Analytics & Advanced Data Analytics",
        org: "Google / Coursera",
        bullets: [
          "Dual professional credentials covering the complete data science and analytics lifecycle — from data cleaning and feature engineering to predictive modeling and executive reporting",
          "Demonstrated mastery in Python, SQL, Tableau, and statistical analysis through rigorous hands-on assessments",
          "Specialized in transforming complex multi-dimensional datasets into stakeholder-ready dashboards and data-driven recommendations"
        ]
      }
    ],
    experience: [
      {
        when: "Ongoing",
        title: "Freelance Data Scientist",
        org: "Fiverr",
        bullets: [
          "Delivering client data cleaning, exploratory analysis, predictive modeling, and executive visualization workflows end-to-end.",
          "Publishing machine learning notebooks, exploratory analyses, and business case studies on Kaggle.",
          "Translating complex multi-dimensional datasets into interactive Tableau dashboards and executive-ready decision frameworks for non-technical stakeholders."
        ]
      },
      {
        when: "Case Studies",
        title: "Data Scientist & Analyst (Self-Directed)",
        org: "Independent Projects",
        bullets: [
          "Completed five end-to-end case studies — Salifort Motors, Cyclistic, Superstore, Fitbit, and PUBG Mobile.",
          "Handled datasets of Customer Churn Prediction (Salifort Motors), leading the full lifecycle from preprocessing and feature engineering to ML modeling and dashboarding.",
          "Trained, hyperparameter-tuned, and evaluated tree-based machine learning classifiers (Decision Trees, Random Forest, XGBoost) achieving up to 99% F1-Score on unseen test data.",
          "Built dynamic Tableau executive dashboards and delivered actionable strategic recommendations to solve real-world churn, retention, and performance problems."
        ]
      }
    ],
    certCard: {
      title: "Google Advanced Data Analytics Professional Certificate",
      subtitle: "Foundational analytics certification"
    }
  },
  contact: {
    email: "mabdullahpro437@gmail.com",
    formEndpoint: "mabdullahpro437@gmail.com",
    linkedin: {
      label: "/in/mabdullahckl",
      url: "https://www.linkedin.com/in/mabdullahckl"
    },
    github: {
      label: "Abdullah-5121",
      url: "https://github.com/Abdullah-5121"
    },
    kaggle: {
      label: "mabdullah5121",
      url: "https://www.kaggle.com/mabdullah5121"
    },
    instagram: {
      label: "mabdullah_ckl",
      url: "https://www.instagram.com/mabdullah_ckl"
    }
  },
  footer: {
    name: "Muhammad Abdullah",
    note: "built with a notebook cell in mind."
  }
};


