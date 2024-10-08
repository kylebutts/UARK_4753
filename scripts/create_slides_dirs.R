create_dir <- function(topic_num, topic_title, topic_stub) {
  raw_doc <- sprintf(
    r'(\documentclass[aspectratio=169,t,11pt,table]{beamer}
\usepackage{../../slides}
\usepackage{../../math}
\definecolor{accent}{HTML}{9D2235}
\definecolor{accent2}{HTML}{2B5269}

\title{Topic %s: %s}
\subtitle{\it  ECON 4753 — University of Arkansas}
\date{Fall 2024}
\author{Prof. Kyle Butts}

\begin{document}

%% ------------------------------------------------------------------------------
\begin{frame}[noframenumbering,plain]
\maketitle

%% \bottomleft{\footnotesize $^*$A bit of extra info here. Add an asterich to title or author}
\end{frame}
%% ------------------------------------------------------------------------------

\begin{frame}{}
  a
\end{frame}

\end{document})',
    topic_num, topic_title
  )

  dir <- sprintf("Slides/%02d_%s", topic_num, topic_stub)
  file <- sprintf("Slides/%02d_%s/%02d_%s.tex", topic_num, topic_stub, topic_num, topic_stub)
  fs::dir_create(dir)
  if (!fs::file_exists(file)) {
    xfun::write_utf8(raw_doc, file)
  }
}

create_dir(
  topic_num = 3,
  topic_title = "Simple Linear Regression",
  topic_stub = "Bivariate_Regression"
)
create_dir(
  topic_num = 4,
  topic_title = "Multiple Regression Analysis",
  topic_stub = "Multiple_Regression"
)
create_dir(
  topic_num = 5,
  topic_title = "Moving Averages and Smoothing Methods",
  topic_stub = "Moving_Averages"
)
create_dir(
  topic_num = 6,
  topic_title = "Regression with Time Series Data",
  topic_stub = "Time_Series_Regression"
)
create_dir(
  topic_num = 7,
  topic_title = "ARIMA Methodology",
  topic_stub = "ARIMA"
)
create_dir(
  topic_num = 8,
  topic_title = "Time Series and Their Components (Optional)",
  topic_stub = "Components_of_Time_Series"
)
