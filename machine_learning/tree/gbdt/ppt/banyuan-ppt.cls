% -*- coding: utf-8 -*-
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{banyuan-ppt}[2016/1/19 v0.1 banyuan-ppt]
\def\version{v0.1}

% option
%% 致谢页显示 
\newif\ifbanyuan@plainthanks\banyuan@plainthanksfalse
\DeclareOption{plainthanks}{
    \banyuan@plainthankstrue
}

%% 字节标题号显示 
\newif\ifbanyuan@showsectionnum\banyuan@showsectionnumfalse
\DeclareOption{showsectionnum}{
    \banyuan@showsectionnumtrue
}

%% Custom package options

%% Fallback
\DeclareOption*{
    \ClassWarning{banyuan-ppt}{Unknown option '\CurrentOption'}
}

%% Execute default options
%\ExecuteOptions{}

%% Process given options
\ProcessOptions\relax

% Load base 
\LoadClass[compress]{beamer}
\mode<presentation>

%% 样式设置
\usetheme{Montpellier}
\hypersetup{pdfpagemode=FullScreen} % makes your presentation go automatically to full screen
\usecolortheme{beaver}
\setbeamertemplate{footline}[text line]{} % makes the footer EMPTY

% Load additional packages and commands.
%% 字体设置
\RequirePackage[BoldFont,SlantFont,CJKchecksingle]{xeCJK}
\setmainfont{Times New Roman}
\setmonofont{Courier New}
\setCJKmainfont[BoldFont=STXinwei]{DFKai-SB} %標楷體，华文新魏
\setCJKmonofont{SimSun}% 设置缺省中文字体

%% 参考文献格式
\setbeamercolor{bibliography entry author}{fg=black}
\setbeamercolor{bibliography entry title}{fg=black} 
\setbeamercolor{bibliography entry location}{fg=black} 
\setbeamercolor{bibliography entry note}{fg=black}  
\setbeamertemplate{bibliography item}{\insertbiblabel}

\RequirePackage{subfigure} %子图
\renewcommand*\thesubfigure{(\alph{subfigure})} 
\RequirePackage{etoolbox}% http://ctan.org/pkg/etoolbox
\AtBeginEnvironment{figure}{\setcounter{subfigure}{0}}% Resets subfigure counter at start of figure environment

\RequirePackage{xcolor}
\RequirePackage{tikz} %draw picture
\usetikzlibrary{shapes.geometric, arrows}
\RequirePackage{pgfplots}
\pgfplotsset{compat=newest}
\pgfplotsset{plot coordinates/math parser=false}
\pgfplotsset{label style={font=\bfseries\Large}}
\pgfplotsset{tick label style={/pgf/number format/1000 sep={}, font=\boldmath\large}}

\RequirePackage{multirow} %跨行表格  
\RequirePackage{booktabs} %table

\RequirePackage{verbatim} %multiple line comment

\RequirePackage{listings} %include soure code

%\RequirePackage{marvosym} %more beautiful symbols
\RequirePackage{wasysym} %checkbox

%% 中文数字
\RequirePackage{CJKnumb}
%% 数学单位
\RequirePackage{siunitx}

% Additional TeX/LaTeX code...
%% pre-set figure width
\newlength{\onepicwidth}
\setlength{\onepicwidth}{0.65\linewidth}
\newlength{\twopicwidth}
\setlength{\twopicwidth}{0.45\linewidth}
\newlength{\threepicwidth}
\setlength{\threepicwidth}{0.3\linewidth}
\newlength{\custompicwidth}

%% 流程图定义基本形状
\tikzstyle{startstop} = [rectangle, rounded corners, minimum width=2cm, minimum height=1cm, text centered, draw=black]
\tikzstyle{io} = [trapezium, trapezium left angle=75, trapezium right angle=105, minimum width=1cm, minimum height=1cm, text centered, draw=black]
\tikzstyle{process} = [rectangle, minimum width=2cm, minimum height=1cm, text centered, draw=black]
\tikzstyle{decision} = [diamond, minimum width=2cm, minimum height=1cm, text centered, draw=black]
\tikzstyle{arrow} = [thick,->,>=stealth]

\renewcommand{\figurename}{图}
\renewcommand{\tablename}{表}
\renewcommand{\today}{\number\year 年 \number\month 月 \number\day 日}

%%% hide all subsubsection
\setcounter{tocdepth}{2}

\ifbanyuan@showsectionnum
    \newenvironment{sectionoutline}{
        \begin{frame}{第\CJKnumber{\arabic{section}}节} %显示小节编号
        }{
        \end{frame}
    }
\else
    \newenvironment{sectionoutline}{
        \begin{frame}
        }{
        \end{frame}
    }
\fi

%%% automatically creat content before every section
\AtBeginSection{
    \begin{sectionoutline}
        \tableofcontents[sections={\thesection}]
    \end{sectionoutline}
    \addtocounter{framenumber}{-1} %目录页不计算页码
}

\DeclareRobustCommand{\createtitle}{
    \begin{frame}[plain]
        \titlepage
    \end{frame}
}

\DeclareRobustCommand{\createoutline}{
    \begin{frame}[plain]{目录}
        \tableofcontents[hideallsubsections]
    \end{frame} 
}

% 致谢页版面
%% 和首页一致
\DeclareRobustCommand{\createlastpageastitle}{
    \begin{frame}[plain]{谢谢！}
        \titlepage
    \end{frame}
}

%% 空白页
\DeclareRobustCommand{\createlastpageasplain}{
    \subsection*{致谢}
    \begin{frame}
        \begin{center}
            \Huge 谢谢！
        \end{center}
    \end{frame}
}

\ifbanyuan@plainthanks
    \def\createlastpage{\createlastpageasplain}
\else
    \def\createlastpage{\createlastpageastitle}
\fi



%todo: if bg exist, then use bg
%{
%\usebackgroundtemplate{%
%    \tikz\node[opacity=0.3] {\includegraphics[height=\paperheight]{./res/halloween.jpg}};}
%\begin{frame}[plain]
%    \titlepage
%\end{frame}
%}

\endinput
