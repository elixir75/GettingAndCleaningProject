



<!DOCTYPE html>
<html lang="en" class=" is-copy-enabled">
  <head prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# object: http://ogp.me/ns/object# article: http://ogp.me/ns/article# profile: http://ogp.me/ns/profile#">
    <meta charset='utf-8'>

    <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/frameworks-486a2ba13121ca853388bbaabfb1482328e14fc6107d2c46137b73a1d2bfb9b7.css" integrity="sha256-SGoroTEhyoUziLuqv7FIIyjhT8YQfSxGE3tzodK/ubc=" media="all" rel="stylesheet" />
    <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github-7bfc60854b4764664fc570a2846b56657937503994a19a5377bdc77f0a19b3f5.css" integrity="sha256-e/xghUtHZGZPxXCihGtWZXk3UDmUoZpTd73HfwoZs/U=" media="all" rel="stylesheet" />
    
    
    <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/site-d2013a78fb2b0b2d9afce49f4cf82965d3f075b6b021bf51aa28d8df766f2ba4.css" integrity="sha256-0gE6ePsrCy2a/OSfTPgpZdPwdbawIb9RqijY33ZvK6Q=" media="all" rel="stylesheet" />
    

    <link as="script" href="https://assets-cdn.github.com/assets/frameworks-e2eca2df0042931f550f59831b5d492c5c279682797c3131a99cd43c3f0917d3.js" rel="preload" />
    
    <link as="script" href="https://assets-cdn.github.com/assets/github-2e43b33c8410732a627bbaf05757eb3584ec9cf7a8747f46ccd2336a28223f60.js" rel="preload" />

    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta http-equiv="Content-Language" content="en">
    <meta name="viewport" content="width=1020">
    
    
    <title>GettingAndCleaningData/run_analysis.Rmd at master · benjamin-chan/GettingAndCleaningData · GitHub</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub">
    <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub">
    <link rel="apple-touch-icon" href="/apple-touch-icon.png">
    <link rel="apple-touch-icon" sizes="57x57" href="/apple-touch-icon-57x57.png">
    <link rel="apple-touch-icon" sizes="60x60" href="/apple-touch-icon-60x60.png">
    <link rel="apple-touch-icon" sizes="72x72" href="/apple-touch-icon-72x72.png">
    <link rel="apple-touch-icon" sizes="76x76" href="/apple-touch-icon-76x76.png">
    <link rel="apple-touch-icon" sizes="114x114" href="/apple-touch-icon-114x114.png">
    <link rel="apple-touch-icon" sizes="120x120" href="/apple-touch-icon-120x120.png">
    <link rel="apple-touch-icon" sizes="144x144" href="/apple-touch-icon-144x144.png">
    <link rel="apple-touch-icon" sizes="152x152" href="/apple-touch-icon-152x152.png">
    <link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon-180x180.png">
    <meta property="fb:app_id" content="1401488693436528">

      <meta content="https://avatars1.githubusercontent.com/u/1897044?v=3&amp;s=400" name="twitter:image:src" /><meta content="@github" name="twitter:site" /><meta content="summary" name="twitter:card" /><meta content="benjamin-chan/GettingAndCleaningData" name="twitter:title" /><meta content="GettingAndCleaningData - Repository for Coursera course Getting and Cleaning Data." name="twitter:description" />
      <meta content="https://avatars1.githubusercontent.com/u/1897044?v=3&amp;s=400" property="og:image" /><meta content="GitHub" property="og:site_name" /><meta content="object" property="og:type" /><meta content="benjamin-chan/GettingAndCleaningData" property="og:title" /><meta content="https://github.com/benjamin-chan/GettingAndCleaningData" property="og:url" /><meta content="GettingAndCleaningData - Repository for Coursera course Getting and Cleaning Data." property="og:description" />
      <meta name="browser-stats-url" content="https://api.github.com/_private/browser/stats">
    <meta name="browser-errors-url" content="https://api.github.com/_private/browser/errors">
    <link rel="assets" href="https://assets-cdn.github.com/">
    
    <meta name="pjax-timeout" content="1000">
    

    <meta name="msapplication-TileImage" content="/windows-tile.png">
    <meta name="msapplication-TileColor" content="#ffffff">
    <meta name="selected-link" value="repo_source" data-pjax-transient>

    <meta name="google-site-verification" content="KT5gs8h0wvaagLKAVWq8bbeNwnZZK1r1XQysX3xurLU">
<meta name="google-site-verification" content="ZzhVyEFwb7w3e0-uOTltm8Jsck2F5StVihD0exw2fsA">
    <meta name="google-analytics" content="UA-3769691-2">

<meta content="collector.githubapp.com" name="octolytics-host" /><meta content="github" name="octolytics-app-id" /><meta content="48B58E47:90F3:236B243:572B026A" name="octolytics-dimension-request_id" />
<meta content="/&lt;user-name&gt;/&lt;repo-name&gt;/blob/show" data-pjax-transient="true" name="analytics-location" />



  <meta class="js-ga-set" name="dimension1" content="Logged Out">



        <meta name="hostname" content="github.com">
    <meta name="user-login" content="">

        <meta name="expected-hostname" content="github.com">
      <meta name="js-proxy-site-detection-payload" content="OGZkNWQ4NjkwMGNmOTQ2NWQyY2QxOGViNzE0YmZlNGM1Zjg3ZTBlZDQ1NjA5MGQ1ZGQ4NzkxMWI1YmE4YWI0OXx7InJlbW90ZV9hZGRyZXNzIjoiNzIuMTgxLjE0Mi43MSIsInJlcXVlc3RfaWQiOiI0OEI1OEU0Nzo5MEYzOjIzNkIyNDM6NTcyQjAyNkEiLCJ0aW1lc3RhbXAiOjE0NjI0MzY0NTh9">


      <link rel="mask-icon" href="https://assets-cdn.github.com/pinned-octocat.svg" color="#4078c0">
      <link rel="icon" type="image/x-icon" href="https://assets-cdn.github.com/favicon.ico">

    <meta content="9cb96e1a5364562890d55471e7d65f206fddf2c1" name="form-nonce" />

    <meta http-equiv="x-pjax-version" content="f9f8c8670e41714c29ab3aa04adcb3c4">
    

      
  <meta name="description" content="GettingAndCleaningData - Repository for Coursera course Getting and Cleaning Data.">
  <meta name="go-import" content="github.com/benjamin-chan/GettingAndCleaningData git https://github.com/benjamin-chan/GettingAndCleaningData.git">

  <meta content="1897044" name="octolytics-dimension-user_id" /><meta content="benjamin-chan" name="octolytics-dimension-user_login" /><meta content="18577753" name="octolytics-dimension-repository_id" /><meta content="benjamin-chan/GettingAndCleaningData" name="octolytics-dimension-repository_nwo" /><meta content="true" name="octolytics-dimension-repository_public" /><meta content="false" name="octolytics-dimension-repository_is_fork" /><meta content="18577753" name="octolytics-dimension-repository_network_root_id" /><meta content="benjamin-chan/GettingAndCleaningData" name="octolytics-dimension-repository_network_root_nwo" />
  <link href="https://github.com/benjamin-chan/GettingAndCleaningData/commits/master.atom" rel="alternate" title="Recent Commits to GettingAndCleaningData:master" type="application/atom+xml">


      <link rel="canonical" href="https://github.com/benjamin-chan/GettingAndCleaningData/blob/master/Project/run_analysis.Rmd" data-pjax-transient>
  </head>


  <body class="logged-out   env-production windows vis-public page-blob">
    <div id="js-pjax-loader-bar" class="pjax-loader-bar"></div>
    <a href="#start-of-content" tabindex="1" class="accessibility-aid js-skip-to-content">Skip to content</a>

    
    
    



          <header class="site-header js-details-container" role="banner">
  <div class="container-responsive">
    <a class="header-logo-invertocat" href="https://github.com/" aria-label="Homepage" data-ga-click="(Logged out) Header, go to homepage, icon:logo-wordmark">
      <svg aria-hidden="true" class="octicon octicon-mark-github" height="32" version="1.1" viewBox="0 0 16 16" width="32"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59 0.4 0.07 0.55-0.17 0.55-0.38 0-0.19-0.01-0.82-0.01-1.49-2.01 0.37-2.53-0.49-2.69-0.94-0.09-0.23-0.48-0.94-0.82-1.13-0.28-0.15-0.68-0.52-0.01-0.53 0.63-0.01 1.08 0.58 1.23 0.82 0.72 1.21 1.87 0.87 2.33 0.66 0.07-0.52 0.28-0.87 0.51-1.07-1.78-0.2-3.64-0.89-3.64-3.95 0-0.87 0.31-1.59 0.82-2.15-0.08-0.2-0.36-1.02 0.08-2.12 0 0 0.67-0.21 2.2 0.82 0.64-0.18 1.32-0.27 2-0.27 0.68 0 1.36 0.09 2 0.27 1.53-1.04 2.2-0.82 2.2-0.82 0.44 1.1 0.16 1.92 0.08 2.12 0.51 0.56 0.82 1.27 0.82 2.15 0 3.07-1.87 3.75-3.65 3.95 0.29 0.25 0.54 0.73 0.54 1.48 0 1.07-0.01 1.93-0.01 2.2 0 0.21 0.15 0.46 0.55 0.38C13.71 14.53 16 11.53 16 8 16 3.58 12.42 0 8 0z"></path></svg>
    </a>

    <button class="btn-link right site-header-toggle js-details-target" type="button" aria-label="Toggle navigation">
      <svg aria-hidden="true" class="octicon octicon-three-bars" height="24" version="1.1" viewBox="0 0 12 16" width="18"><path d="M11.41 9H0.59c-0.59 0-0.59-0.41-0.59-1s0-1 0.59-1h10.81c0.59 0 0.59 0.41 0.59 1s0 1-0.59 1z m0-4H0.59c-0.59 0-0.59-0.41-0.59-1s0-1 0.59-1h10.81c0.59 0 0.59 0.41 0.59 1s0 1-0.59 1zM0.59 11h10.81c0.59 0 0.59 0.41 0.59 1s0 1-0.59 1H0.59c-0.59 0-0.59-0.41-0.59-1s0-1 0.59-1z"></path></svg>
    </button>

    <div class="site-header-menu">
      <nav class="site-header-nav site-header-nav-main">
        <a href="/personal" class="js-selected-navigation-item nav-item nav-item-personal" data-ga-click="Header, click, Nav menu - item:personal" data-selected-links="/personal /personal">
          Personal
</a>        <a href="/open-source" class="js-selected-navigation-item nav-item nav-item-opensource" data-ga-click="Header, click, Nav menu - item:opensource" data-selected-links="/open-source /open-source">
          Open source
</a>        <a href="/business" class="js-selected-navigation-item nav-item nav-item-business" data-ga-click="Header, click, Nav menu - item:business" data-selected-links="/business /business/features /business/customers /business">
          Business
</a>        <a href="/explore" class="js-selected-navigation-item nav-item nav-item-explore" data-ga-click="Header, click, Nav menu - item:explore" data-selected-links="/explore /trending /trending/developers /integrations /integrations/feature/code /integrations/feature/collaborate /integrations/feature/ship /explore">
          Explore
</a>      </nav>

      <div class="site-header-actions">
            <a class="btn btn-primary site-header-actions-btn" href="/join?source=header-repo" data-ga-click="(Logged out) Header, clicked Sign up, text:sign-up">Sign up</a>
          <a class="btn site-header-actions-btn mr-2" href="/login?return_to=%2Fbenjamin-chan%2FGettingAndCleaningData%2Fblob%2Fmaster%2FProject%2Frun_analysis.Rmd" data-ga-click="(Logged out) Header, clicked Sign in, text:sign-in">Sign in</a>
      </div>

        <nav class="site-header-nav site-header-nav-secondary">
          <a class="nav-item" href="/pricing">Pricing</a>
          <a class="nav-item" href="/blog">Blog</a>
          <a class="nav-item" href="https://help.github.com">Support</a>
          <a class="nav-item header-search-link" href="https://github.com/search">Search GitHub</a>
              <div class="header-search scoped-search site-scoped-search js-site-search" role="search">
  <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="/benjamin-chan/GettingAndCleaningData/search" class="js-site-search-form" data-scoped-search-url="/benjamin-chan/GettingAndCleaningData/search" data-unscoped-search-url="/search" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
    <label class="form-control header-search-wrapper js-chromeless-input-container">
      <div class="header-search-scope">This repository</div>
      <input type="text"
        class="form-control header-search-input js-site-search-focus js-site-search-field is-clearable"
        data-hotkey="s"
        name="q"
        placeholder="Search"
        aria-label="Search this repository"
        data-unscoped-placeholder="Search GitHub"
        data-scoped-placeholder="Search"
        tabindex="1"
        autocapitalize="off">
    </label>
</form></div>

        </nav>
    </div>
  </div>
</header>



    <div id="start-of-content" class="accessibility-aid"></div>

      <div id="js-flash-container">
</div>


    <div role="main" class="main-content">
        <div itemscope itemtype="http://schema.org/SoftwareSourceCode">
    <div id="js-repo-pjax-container" data-pjax-container>
      
<div class="pagehead repohead instapaper_ignore readability-menu experiment-repo-nav">
  <div class="container repohead-details-container">

    

<ul class="pagehead-actions">

  <li>
      <a href="/login?return_to=%2Fbenjamin-chan%2FGettingAndCleaningData"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to watch a repository" rel="nofollow">
    <svg aria-hidden="true" class="octicon octicon-eye" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M8.06 2C3 2 0 8 0 8s3 6 8.06 6c4.94 0 7.94-6 7.94-6S13 2 8.06 2z m-0.06 10c-2.2 0-4-1.78-4-4 0-2.2 1.8-4 4-4 2.22 0 4 1.8 4 4 0 2.22-1.78 4-4 4z m2-4c0 1.11-0.89 2-2 2s-2-0.89-2-2 0.89-2 2-2 2 0.89 2 2z"></path></svg>
    Watch
  </a>
  <a class="social-count" href="/benjamin-chan/GettingAndCleaningData/watchers">
    14
  </a>

  </li>

  <li>
      <a href="/login?return_to=%2Fbenjamin-chan%2FGettingAndCleaningData"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to star a repository" rel="nofollow">
    <svg aria-hidden="true" class="octicon octicon-star" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path d="M14 6l-4.9-0.64L7 1 4.9 5.36 0 6l3.6 3.26L2.67 14l4.33-2.33 4.33 2.33L10.4 9.26 14 6z"></path></svg>
    Star
  </a>

    <a class="social-count js-social-count" href="/benjamin-chan/GettingAndCleaningData/stargazers">
      46
    </a>

  </li>

  <li>
      <a href="/login?return_to=%2Fbenjamin-chan%2FGettingAndCleaningData"
        class="btn btn-sm btn-with-count tooltipped tooltipped-n"
        aria-label="You must be signed in to fork a repository" rel="nofollow">
        <svg aria-hidden="true" class="octicon octicon-repo-forked" height="16" version="1.1" viewBox="0 0 10 16" width="10"><path d="M8 1c-1.11 0-2 0.89-2 2 0 0.73 0.41 1.38 1 1.72v1.28L5 8 3 6v-1.28c0.59-0.34 1-0.98 1-1.72 0-1.11-0.89-2-2-2S0 1.89 0 3c0 0.73 0.41 1.38 1 1.72v1.78l3 3v1.78c-0.59 0.34-1 0.98-1 1.72 0 1.11 0.89 2 2 2s2-0.89 2-2c0-0.73-0.41-1.38-1-1.72V9.5l3-3V4.72c0.59-0.34 1-0.98 1-1.72 0-1.11-0.89-2-2-2zM2 4.2c-0.66 0-1.2-0.55-1.2-1.2s0.55-1.2 1.2-1.2 1.2 0.55 1.2 1.2-0.55 1.2-1.2 1.2z m3 10c-0.66 0-1.2-0.55-1.2-1.2s0.55-1.2 1.2-1.2 1.2 0.55 1.2 1.2-0.55 1.2-1.2 1.2z m3-10c-0.66 0-1.2-0.55-1.2-1.2s0.55-1.2 1.2-1.2 1.2 0.55 1.2 1.2-0.55 1.2-1.2 1.2z"></path></svg>
        Fork
      </a>

    <a href="/benjamin-chan/GettingAndCleaningData/network" class="social-count">
      282
    </a>
  </li>
</ul>

    <h1 class="entry-title public ">
  <svg aria-hidden="true" class="octicon octicon-repo" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path d="M4 9h-1v-1h1v1z m0-3h-1v1h1v-1z m0-2h-1v1h1v-1z m0-2h-1v1h1v-1z m8-1v12c0 0.55-0.45 1-1 1H6v2l-1.5-1.5-1.5 1.5V14H1c-0.55 0-1-0.45-1-1V1C0 0.45 0.45 0 1 0h10c0.55 0 1 0.45 1 1z m-1 10H1v2h2v-1h3v1h5V11z m0-10H2v9h9V1z"></path></svg>
  <span class="author" itemprop="author"><a href="/benjamin-chan" class="url fn" rel="author">benjamin-chan</a></span><!--
--><span class="path-divider">/</span><!--
--><strong itemprop="name"><a href="/benjamin-chan/GettingAndCleaningData" data-pjax="#js-repo-pjax-container">GettingAndCleaningData</a></strong>

</h1>

  </div>
  <div class="container">
    
<nav class="reponav js-repo-nav js-sidenav-container-pjax"
     itemscope
     itemtype="http://schema.org/BreadcrumbList"
     role="navigation"
     data-pjax="#js-repo-pjax-container">

  <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
    <a href="/benjamin-chan/GettingAndCleaningData" aria-selected="true" class="js-selected-navigation-item selected reponav-item" data-hotkey="g c" data-selected-links="repo_source repo_downloads repo_commits repo_releases repo_tags repo_branches /benjamin-chan/GettingAndCleaningData" itemprop="url">
      <svg aria-hidden="true" class="octicon octicon-code" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path d="M9.5 3l-1.5 1.5 3.5 3.5L8 11.5l1.5 1.5 4.5-5L9.5 3zM4.5 3L0 8l4.5 5 1.5-1.5L2.5 8l3.5-3.5L4.5 3z"></path></svg>
      <span itemprop="name">Code</span>
      <meta itemprop="position" content="1">
</a>  </span>

    <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
      <a href="/benjamin-chan/GettingAndCleaningData/issues" class="js-selected-navigation-item reponav-item" data-hotkey="g i" data-selected-links="repo_issues repo_labels repo_milestones /benjamin-chan/GettingAndCleaningData/issues" itemprop="url">
        <svg aria-hidden="true" class="octicon octicon-issue-opened" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path d="M7 2.3c3.14 0 5.7 2.56 5.7 5.7S10.14 13.7 7 13.7 1.3 11.14 1.3 8s2.56-5.7 5.7-5.7m0-1.3C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14 7-7S10.86 1 7 1z m1 3H6v5h2V4z m0 6H6v2h2V10z"></path></svg>
        <span itemprop="name">Issues</span>
        <span class="counter">0</span>
        <meta itemprop="position" content="2">
</a>    </span>

  <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
    <a href="/benjamin-chan/GettingAndCleaningData/pulls" class="js-selected-navigation-item reponav-item" data-hotkey="g p" data-selected-links="repo_pulls /benjamin-chan/GettingAndCleaningData/pulls" itemprop="url">
      <svg aria-hidden="true" class="octicon octicon-git-pull-request" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path d="M11 11.28c0-1.73 0-6.28 0-6.28-0.03-0.78-0.34-1.47-0.94-2.06s-1.28-0.91-2.06-0.94c0 0-1.02 0-1 0V0L4 3l3 3V4h1c0.27 0.02 0.48 0.11 0.69 0.31s0.3 0.42 0.31 0.69v6.28c-0.59 0.34-1 0.98-1 1.72 0 1.11 0.89 2 2 2s2-0.89 2-2c0-0.73-0.41-1.38-1-1.72z m-1 2.92c-0.66 0-1.2-0.55-1.2-1.2s0.55-1.2 1.2-1.2 1.2 0.55 1.2 1.2-0.55 1.2-1.2 1.2zM4 3c0-1.11-0.89-2-2-2S0 1.89 0 3c0 0.73 0.41 1.38 1 1.72 0 1.55 0 5.56 0 6.56-0.59 0.34-1 0.98-1 1.72 0 1.11 0.89 2 2 2s2-0.89 2-2c0-0.73-0.41-1.38-1-1.72V4.72c0.59-0.34 1-0.98 1-1.72z m-0.8 10c0 0.66-0.55 1.2-1.2 1.2s-1.2-0.55-1.2-1.2 0.55-1.2 1.2-1.2 1.2 0.55 1.2 1.2z m-1.2-8.8c-0.66 0-1.2-0.55-1.2-1.2s0.55-1.2 1.2-1.2 1.2 0.55 1.2 1.2-0.55 1.2-1.2 1.2z"></path></svg>
      <span itemprop="name">Pull requests</span>
      <span class="counter">0</span>
      <meta itemprop="position" content="3">
</a>  </span>



  <a href="/benjamin-chan/GettingAndCleaningData/pulse" class="js-selected-navigation-item reponav-item" data-selected-links="pulse /benjamin-chan/GettingAndCleaningData/pulse">
    <svg aria-hidden="true" class="octicon octicon-pulse" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path d="M11.5 8L8.8 5.4 6.6 8.5 5.5 1.6 2.38 8H0V10h3.6L4.5 8.2l0.9 5.4L9 8.5l1.6 1.5H14V8H11.5z"></path></svg>
    Pulse
</a>
  <a href="/benjamin-chan/GettingAndCleaningData/graphs" class="js-selected-navigation-item reponav-item" data-selected-links="repo_graphs repo_contributors /benjamin-chan/GettingAndCleaningData/graphs">
    <svg aria-hidden="true" class="octicon octicon-graph" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M16 14v1H0V0h1v14h15z m-11-1H3V8h2v5z m4 0H7V3h2v10z m4 0H11V6h2v7z"></path></svg>
    Graphs
</a>

</nav>

  </div>
</div>

<div class="container new-discussion-timeline experiment-repo-nav">
  <div class="repository-content">

    

<a href="/benjamin-chan/GettingAndCleaningData/blob/eb401a34579a545bc64fedd8d410bdf8ecb6f992/Project/run_analysis.Rmd" class="hidden js-permalink-shortcut" data-hotkey="y">Permalink</a>

<!-- blob contrib key: blob_contributors:v21:6db8e64c1291e150fb1f7e559b7434d9 -->

<div class="file-navigation js-zeroclipboard-container">
  
<div class="select-menu branch-select-menu js-menu-container js-select-menu left">
  <button class="btn btn-sm select-menu-button js-menu-target css-truncate" data-hotkey="w"
    title="master"
    type="button" aria-label="Switch branches or tags" tabindex="0" aria-haspopup="true">
    <i>Branch:</i>
    <span class="js-select-button css-truncate-target">master</span>
  </button>

  <div class="select-menu-modal-holder js-menu-content js-navigation-container" data-pjax aria-hidden="true">

    <div class="select-menu-modal">
      <div class="select-menu-header">
        <svg aria-label="Close" class="octicon octicon-x js-menu-close" height="16" role="img" version="1.1" viewBox="0 0 12 16" width="12"><path d="M7.48 8l3.75 3.75-1.48 1.48-3.75-3.75-3.75 3.75-1.48-1.48 3.75-3.75L0.77 4.25l1.48-1.48 3.75 3.75 3.75-3.75 1.48 1.48-3.75 3.75z"></path></svg>
        <span class="select-menu-title">Switch branches/tags</span>
      </div>

      <div class="select-menu-filters">
        <div class="select-menu-text-filter">
          <input type="text" aria-label="Filter branches/tags" id="context-commitish-filter-field" class="form-control js-filterable-field js-navigation-enable" placeholder="Filter branches/tags">
        </div>
        <div class="select-menu-tabs">
          <ul>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="branches" data-filter-placeholder="Filter branches/tags" class="js-select-menu-tab" role="tab">Branches</a>
            </li>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="tags" data-filter-placeholder="Find a tag…" class="js-select-menu-tab" role="tab">Tags</a>
            </li>
          </ul>
        </div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="branches" role="menu">

        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <a class="select-menu-item js-navigation-item js-navigation-open selected"
               href="/benjamin-chan/GettingAndCleaningData/blob/master/Project/run_analysis.Rmd"
               data-name="master"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path d="M12 5L4 13 0 9l1.5-1.5 2.5 2.5 6.5-6.5 1.5 1.5z"></path></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text" title="master">
                master
              </span>
            </a>
        </div>

          <div class="select-menu-no-results">Nothing to show</div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="tags">
        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


        </div>

        <div class="select-menu-no-results">Nothing to show</div>
      </div>

    </div>
  </div>
</div>

  <div class="btn-group right">
    <a href="/benjamin-chan/GettingAndCleaningData/find/master"
          class="js-pjax-capture-input btn btn-sm"
          data-pjax
          data-hotkey="t">
      Find file
    </a>
    <button aria-label="Copy file path to clipboard" class="js-zeroclipboard btn btn-sm zeroclipboard-button tooltipped tooltipped-s" data-copied-hint="Copied!" type="button">Copy path</button>
  </div>
  <div class="breadcrumb js-zeroclipboard-target">
    <span class="repo-root js-repo-root"><span class="js-path-segment"><a href="/benjamin-chan/GettingAndCleaningData"><span>GettingAndCleaningData</span></a></span></span><span class="separator">/</span><span class="js-path-segment"><a href="/benjamin-chan/GettingAndCleaningData/tree/master/Project"><span>Project</span></a></span><span class="separator">/</span><strong class="final-path">run_analysis.Rmd</strong>
  </div>
</div>


  <div class="commit-tease">
      <span class="right">
        <a class="commit-tease-sha" href="/benjamin-chan/GettingAndCleaningData/commit/73f0abb7e5bd770a824ee189e21830c7485c5f1a" data-pjax>
          73f0abb
        </a>
        <relative-time datetime="2014-04-17T16:10:14Z">Apr 17, 2014</relative-time>
      </span>
      <div>
        <img alt="@benjamin-chan" class="avatar" height="20" src="https://avatars1.githubusercontent.com/u/1897044?v=3&amp;s=40" width="20" />
        <a href="/benjamin-chan" class="user-mention" rel="author">benjamin-chan</a>
          <a href="/benjamin-chan/GettingAndCleaningData/commit/73f0abb7e5bd770a824ee189e21830c7485c5f1a" class="message" data-pjax="true" title="Add codebook.html">Add codebook.html</a>
      </div>

    <div class="commit-tease-contributors">
      <button type="button" class="btn-link muted-link contributors-toggle" data-facebox="#blob_contributors_box">
        <strong>1</strong>
         contributor
      </button>
      
    </div>

    <div id="blob_contributors_box" style="display:none">
      <h2 class="facebox-header" data-facebox-id="facebox-header">Users who have contributed to this file</h2>
      <ul class="facebox-user-list" data-facebox-id="facebox-description">
          <li class="facebox-user-list-item">
            <img alt="@benjamin-chan" height="24" src="https://avatars3.githubusercontent.com/u/1897044?v=3&amp;s=48" width="24" />
            <a href="/benjamin-chan">benjamin-chan</a>
          </li>
      </ul>
    </div>
  </div>

<div class="file">
  <div class="file-header">
  <div class="file-actions">

    <div class="btn-group">
      <a href="/benjamin-chan/GettingAndCleaningData/raw/master/Project/run_analysis.Rmd" class="btn btn-sm " id="raw-url">Raw</a>
        <a href="/benjamin-chan/GettingAndCleaningData/blame/master/Project/run_analysis.Rmd" class="btn btn-sm js-update-url-with-hash">Blame</a>
      <a href="/benjamin-chan/GettingAndCleaningData/commits/master/Project/run_analysis.Rmd" class="btn btn-sm " rel="nofollow">History</a>
    </div>

        <a class="btn-octicon tooltipped tooltipped-nw"
           href="https://windows.github.com"
           aria-label="Open this file in GitHub Desktop"
           data-ga-click="Repository, open with desktop, type:windows">
            <svg aria-hidden="true" class="octicon octicon-device-desktop" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M15 2H1c-0.55 0-1 0.45-1 1v9c0 0.55 0.45 1 1 1h5.34c-0.25 0.61-0.86 1.39-2.34 2h8c-1.48-0.61-2.09-1.39-2.34-2h5.34c0.55 0 1-0.45 1-1V3c0-0.55-0.45-1-1-1z m0 9H1V3h14v8z"></path></svg>
        </a>

        <button type="button" class="btn-octicon disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <svg aria-hidden="true" class="octicon octicon-pencil" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path d="M0 12v3h3l8-8-3-3L0 12z m3 2H1V12h1v1h1v1z m10.3-9.3l-1.3 1.3-3-3 1.3-1.3c0.39-0.39 1.02-0.39 1.41 0l1.59 1.59c0.39 0.39 0.39 1.02 0 1.41z"></path></svg>
        </button>
        <button type="button" class="btn-octicon btn-octicon-danger disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <svg aria-hidden="true" class="octicon octicon-trashcan" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path d="M10 2H8c0-0.55-0.45-1-1-1H4c-0.55 0-1 0.45-1 1H1c-0.55 0-1 0.45-1 1v1c0 0.55 0.45 1 1 1v9c0 0.55 0.45 1 1 1h7c0.55 0 1-0.45 1-1V5c0.55 0 1-0.45 1-1v-1c0-0.55-0.45-1-1-1z m-1 12H2V5h1v8h1V5h1v8h1V5h1v8h1V5h1v9z m1-10H1v-1h9v1z"></path></svg>
        </button>
  </div>

  <div class="file-info">
      272 lines (197 sloc)
      <span class="file-info-divider"></span>
    8.72 KB
  </div>
</div>

  
  <div id="readme" class="readme blob instapaper_body">
    <article class="markdown-body entry-content" itemprop="text"><h1><a id="user-content-run_analysis" class="anchor" href="#run_analysis" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>run_analysis</h1>

<p>Last updated <code>r as.character(Sys.time())</code> using <code>r R.version$version.string</code>.</p>

<h2><a id="user-content-instructions-for-project" class="anchor" href="#instructions-for-project" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>Instructions for project</h2>

<blockquote>
<p>The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  </p>

<p>One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: </p>

<p><a href="http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones">http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones</a> </p>

<p>Here are the data for the project: </p>

<p><a href="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip">https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip</a> </p>

<p>You should create one R script called run_analysis.R that does the following. </p>

<ol>
<li><strong>DONE</strong> Merges the training and the test sets to create one data set.</li>
<li><strong>DONE</strong> Extracts only the measurements on the mean and standard deviation for each measurement.</li>
<li><strong>DONE</strong> Uses descriptive activity names to name the activities in the data set.</li>
<li><strong>DONE</strong> Appropriately labels the data set with descriptive activity names.</li>
<li><strong>DONE</strong> Creates a second, independent tidy data set with the average of each variable for each activity and each subject. </li>
</ol>

<p>Good luck!</p>
</blockquote>

<p><strong>The codebook is at the end of this document.</strong></p>

<h2><a id="user-content-preliminaries" class="anchor" href="#preliminaries" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>Preliminaries</h2>

<p>Load packages.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">packages</span> <span class="pl-k">&lt;-</span> c(<span class="pl-s"><span class="pl-pds">"</span>data.table<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>reshape2<span class="pl-pds">"</span></span>)
sapply(<span class="pl-smi">packages</span>, <span class="pl-smi">require</span>, <span class="pl-v">character.only</span><span class="pl-k">=</span><span class="pl-c1">TRUE</span>, <span class="pl-v">quietly</span><span class="pl-k">=</span><span class="pl-c1">TRUE</span>)</pre></div>

<p>Set path.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">path</span> <span class="pl-k">&lt;-</span> getwd()
<span class="pl-smi">path</span></pre></div>

<h2><a id="user-content-get-the-data" class="anchor" href="#get-the-data" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>Get the data</h2>

<p>Download the file. Put it in the <code>Data</code> folder. <strong>This was already done on 2014-04-11; save time and don't evaluate again.</strong></p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">url</span> <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">"</span>https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip<span class="pl-pds">"</span></span>
<span class="pl-smi">f</span> <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">"</span>Dataset.zip<span class="pl-pds">"</span></span>
<span class="pl-k">if</span> (<span class="pl-k">!</span>file.exists(<span class="pl-smi">path</span>)) {dir.create(<span class="pl-smi">path</span>)}
download.file(<span class="pl-smi">url</span>, file.path(<span class="pl-smi">path</span>, <span class="pl-smi">f</span>))</pre></div>

<p>Unzip the file. <strong>This was already done on 2014-04-11; save time and don't evaluate again.</strong></p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">executable</span> <span class="pl-k">&lt;-</span> file.path(<span class="pl-s"><span class="pl-pds">"</span>C:<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>Program Files (x86)<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>7-Zip<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>7z.exe<span class="pl-pds">"</span></span>)
<span class="pl-smi">parameters</span> <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">"</span>x<span class="pl-pds">"</span></span>
<span class="pl-smi">cmd</span> <span class="pl-k">&lt;-</span> paste(paste0(<span class="pl-s"><span class="pl-pds">"</span><span class="pl-cce">\"</span><span class="pl-pds">"</span></span>, <span class="pl-smi">executable</span>, <span class="pl-s"><span class="pl-pds">"</span><span class="pl-cce">\"</span><span class="pl-pds">"</span></span>), <span class="pl-smi">parameters</span>, paste0(<span class="pl-s"><span class="pl-pds">"</span><span class="pl-cce">\"</span><span class="pl-pds">"</span></span>, file.path(<span class="pl-smi">path</span>, <span class="pl-smi">f</span>), <span class="pl-s"><span class="pl-pds">"</span><span class="pl-cce">\"</span><span class="pl-pds">"</span></span>))
system(<span class="pl-smi">cmd</span>)</pre></div>

<p>The archive put the files in a folder named <code>UCI HAR Dataset</code>. Set this folder as the input path. List the files here.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">pathIn</span> <span class="pl-k">&lt;-</span> file.path(<span class="pl-smi">path</span>, <span class="pl-s"><span class="pl-pds">"</span>UCI HAR Dataset<span class="pl-pds">"</span></span>)
list.files(<span class="pl-smi">pathIn</span>, <span class="pl-v">recursive</span><span class="pl-k">=</span><span class="pl-c1">TRUE</span>)</pre></div>

<p><strong>See the <code>README.txt</code> file in <code>r path</code> for detailed information on the dataset.</strong></p>

<p>For the purposes of this project, the files in the <code>Inertial Signals</code> folders are not used.</p>

<h2><a id="user-content-read-the-files" class="anchor" href="#read-the-files" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>Read the files</h2>

<p>Read the subject files.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dtSubjectTrain</span> <span class="pl-k">&lt;-</span> fread(file.path(<span class="pl-smi">pathIn</span>, <span class="pl-s"><span class="pl-pds">"</span>train<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>subject_train.txt<span class="pl-pds">"</span></span>))
<span class="pl-smi">dtSubjectTest</span>  <span class="pl-k">&lt;-</span> fread(file.path(<span class="pl-smi">pathIn</span>, <span class="pl-s"><span class="pl-pds">"</span>test<span class="pl-pds">"</span></span> , <span class="pl-s"><span class="pl-pds">"</span>subject_test.txt<span class="pl-pds">"</span></span> ))</pre></div>

<p>Read the activity files. For some reason, these are called <em>label</em> files in the <code>README.txt</code> documentation.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dtActivityTrain</span> <span class="pl-k">&lt;-</span> fread(file.path(<span class="pl-smi">pathIn</span>, <span class="pl-s"><span class="pl-pds">"</span>train<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>Y_train.txt<span class="pl-pds">"</span></span>))
<span class="pl-smi">dtActivityTest</span>  <span class="pl-k">&lt;-</span> fread(file.path(<span class="pl-smi">pathIn</span>, <span class="pl-s"><span class="pl-pds">"</span>test<span class="pl-pds">"</span></span> , <span class="pl-s"><span class="pl-pds">"</span>Y_test.txt<span class="pl-pds">"</span></span> ))</pre></div>

<p>Read the data files. <code>fread</code> seems to be giving me some trouble reading files. Using a helper function, read the file with <code>read.table</code> instead, then convert the resulting data frame to a data table. Return the data table.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-en">fileToDataTable</span> <span class="pl-k">&lt;-</span> <span class="pl-k">function</span> (<span class="pl-smi">f</span>) {
    <span class="pl-smi">df</span> <span class="pl-k">&lt;-</span> read.table(<span class="pl-smi">f</span>)
    <span class="pl-smi">dt</span> <span class="pl-k">&lt;-</span> data.table(<span class="pl-smi">df</span>)
}
<span class="pl-smi">dtTrain</span> <span class="pl-k">&lt;-</span> fileToDataTable(file.path(<span class="pl-smi">pathIn</span>, <span class="pl-s"><span class="pl-pds">"</span>train<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>X_train.txt<span class="pl-pds">"</span></span>))
<span class="pl-smi">dtTest</span>  <span class="pl-k">&lt;-</span> fileToDataTable(file.path(<span class="pl-smi">pathIn</span>, <span class="pl-s"><span class="pl-pds">"</span>test<span class="pl-pds">"</span></span> , <span class="pl-s"><span class="pl-pds">"</span>X_test.txt<span class="pl-pds">"</span></span> ))</pre></div>

<h2><a id="user-content-merge-the-training-and-the-test-sets" class="anchor" href="#merge-the-training-and-the-test-sets" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>Merge the training and the test sets</h2>

<p>Concatenate the data tables.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dtSubject</span> <span class="pl-k">&lt;-</span> rbind(<span class="pl-smi">dtSubjectTrain</span>, <span class="pl-smi">dtSubjectTest</span>)
setnames(<span class="pl-smi">dtSubject</span>, <span class="pl-s"><span class="pl-pds">"</span>V1<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>subject<span class="pl-pds">"</span></span>)
<span class="pl-smi">dtActivity</span> <span class="pl-k">&lt;-</span> rbind(<span class="pl-smi">dtActivityTrain</span>, <span class="pl-smi">dtActivityTest</span>)
setnames(<span class="pl-smi">dtActivity</span>, <span class="pl-s"><span class="pl-pds">"</span>V1<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>activityNum<span class="pl-pds">"</span></span>)
<span class="pl-smi">dt</span> <span class="pl-k">&lt;-</span> rbind(<span class="pl-smi">dtTrain</span>, <span class="pl-smi">dtTest</span>)</pre></div>

<p>Merge columns.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dtSubject</span> <span class="pl-k">&lt;-</span> cbind(<span class="pl-smi">dtSubject</span>, <span class="pl-smi">dtActivity</span>)
<span class="pl-smi">dt</span> <span class="pl-k">&lt;-</span> cbind(<span class="pl-smi">dtSubject</span>, <span class="pl-smi">dt</span>)</pre></div>

<p>Set key.</p>

<div class="highlight highlight-source-r"><pre>setkey(<span class="pl-smi">dt</span>, <span class="pl-smi">subject</span>, <span class="pl-smi">activityNum</span>)</pre></div>

<h2><a id="user-content-extract-only-the-mean-and-standard-deviation" class="anchor" href="#extract-only-the-mean-and-standard-deviation" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>Extract only the mean and standard deviation</h2>

<p>Read the <code>features.txt</code> file. This tells which variables in <code>dt</code> are measurements for the mean and standard deviation.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dtFeatures</span> <span class="pl-k">&lt;-</span> fread(file.path(<span class="pl-smi">pathIn</span>, <span class="pl-s"><span class="pl-pds">"</span>features.txt<span class="pl-pds">"</span></span>))
setnames(<span class="pl-smi">dtFeatures</span>, names(<span class="pl-smi">dtFeatures</span>), c(<span class="pl-s"><span class="pl-pds">"</span>featureNum<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>featureName<span class="pl-pds">"</span></span>))</pre></div>

<p>Subset only measurements for the mean and standard deviation.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dtFeatures</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">dtFeatures</span>[grepl(<span class="pl-s"><span class="pl-pds">"</span>mean<span class="pl-cce">\\</span>(<span class="pl-cce">\\</span>)|std<span class="pl-cce">\\</span>(<span class="pl-cce">\\</span>)<span class="pl-pds">"</span></span>, <span class="pl-smi">featureName</span>)]</pre></div>

<p>Convert the column numbers to a vector of variable names matching columns in <code>dt</code>.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dtFeatures</span><span class="pl-k">$</span><span class="pl-smi">featureCode</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">dtFeatures</span>[, paste0(<span class="pl-s"><span class="pl-pds">"</span>V<span class="pl-pds">"</span></span>, <span class="pl-smi">featureNum</span>)]
head(<span class="pl-smi">dtFeatures</span>)
<span class="pl-smi">dtFeatures</span><span class="pl-k">$</span><span class="pl-smi">featureCode</span></pre></div>

<p>Subset these variables using variable names.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">select</span> <span class="pl-k">&lt;-</span> c(key(<span class="pl-smi">dt</span>), <span class="pl-smi">dtFeatures</span><span class="pl-k">$</span><span class="pl-smi">featureCode</span>)
<span class="pl-smi">dt</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">dt</span>[, <span class="pl-smi">select</span>, <span class="pl-v">with</span><span class="pl-k">=</span><span class="pl-c1">FALSE</span>]</pre></div>

<h2><a id="user-content-use-descriptive-activity-names" class="anchor" href="#use-descriptive-activity-names" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>Use descriptive activity names</h2>

<p>Read <code>activity_labels.txt</code> file. This will be used to add descriptive names to the activities.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dtActivityNames</span> <span class="pl-k">&lt;-</span> fread(file.path(<span class="pl-smi">pathIn</span>, <span class="pl-s"><span class="pl-pds">"</span>activity_labels.txt<span class="pl-pds">"</span></span>))
setnames(<span class="pl-smi">dtActivityNames</span>, names(<span class="pl-smi">dtActivityNames</span>), c(<span class="pl-s"><span class="pl-pds">"</span>activityNum<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>activityName<span class="pl-pds">"</span></span>))</pre></div>

<h2><a id="user-content-label-with-descriptive-activity-names" class="anchor" href="#label-with-descriptive-activity-names" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>Label with descriptive activity names</h2>

<p>Merge activity labels.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dt</span> <span class="pl-k">&lt;-</span> merge(<span class="pl-smi">dt</span>, <span class="pl-smi">dtActivityNames</span>, <span class="pl-v">by</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">"</span>activityNum<span class="pl-pds">"</span></span>, <span class="pl-v">all.x</span><span class="pl-k">=</span><span class="pl-c1">TRUE</span>)</pre></div>

<p>Add <code>activityName</code> as a key.</p>

<div class="highlight highlight-source-r"><pre>setkey(<span class="pl-smi">dt</span>, <span class="pl-smi">subject</span>, <span class="pl-smi">activityNum</span>, <span class="pl-smi">activityName</span>)</pre></div>

<p>Melt the data table to reshape it from a short and wide format to a tall and narrow format.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dt</span> <span class="pl-k">&lt;-</span> data.table(melt(<span class="pl-smi">dt</span>, key(<span class="pl-smi">dt</span>), <span class="pl-v">variable.name</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">"</span>featureCode<span class="pl-pds">"</span></span>))</pre></div>

<p>Merge activity name.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dt</span> <span class="pl-k">&lt;-</span> merge(<span class="pl-smi">dt</span>, <span class="pl-smi">dtFeatures</span>[, <span class="pl-k">list</span>(<span class="pl-smi">featureNum</span>, <span class="pl-smi">featureCode</span>, <span class="pl-smi">featureName</span>)], <span class="pl-v">by</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">"</span>featureCode<span class="pl-pds">"</span></span>, <span class="pl-v">all.x</span><span class="pl-k">=</span><span class="pl-c1">TRUE</span>)</pre></div>

<p>Create a new variable, <code>activity</code> that is equivalent to <code>activityName</code> as a factor class.
Create a new variable, <code>feature</code> that is equivalent to <code>featureName</code> as a factor class.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">activity</span> <span class="pl-k">&lt;-</span> <span class="pl-k">factor</span>(<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">activityName</span>)
<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">feature</span> <span class="pl-k">&lt;-</span> <span class="pl-k">factor</span>(<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">featureName</span>)</pre></div>

<p>Seperate features from <code>featureName</code> using the helper function <code>grepthis</code>.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-en">grepthis</span> <span class="pl-k">&lt;-</span> <span class="pl-k">function</span> (<span class="pl-smi">regex</span>) {
  grepl(<span class="pl-smi">regex</span>, <span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">feature</span>)
}
<span class="pl-c">## Features with 2 categories</span>
<span class="pl-smi">n</span> <span class="pl-k">&lt;-</span> <span class="pl-c1">2</span>
<span class="pl-smi">y</span> <span class="pl-k">&lt;-</span> <span class="pl-k">matrix</span>(seq(<span class="pl-c1">1</span>, <span class="pl-smi">n</span>), <span class="pl-v">nrow</span><span class="pl-k">=</span><span class="pl-smi">n</span>)
<span class="pl-smi">x</span> <span class="pl-k">&lt;-</span> <span class="pl-k">matrix</span>(c(grepthis(<span class="pl-s"><span class="pl-pds">"</span>^t<span class="pl-pds">"</span></span>), grepthis(<span class="pl-s"><span class="pl-pds">"</span>^f<span class="pl-pds">"</span></span>)), <span class="pl-v">ncol</span><span class="pl-k">=</span>nrow(<span class="pl-smi">y</span>))
<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">featDomain</span> <span class="pl-k">&lt;-</span> <span class="pl-k">factor</span>(<span class="pl-smi">x</span> <span class="pl-k">%*%</span> <span class="pl-smi">y</span>, <span class="pl-v">labels</span><span class="pl-k">=</span>c(<span class="pl-s"><span class="pl-pds">"</span>Time<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>Freq<span class="pl-pds">"</span></span>))
<span class="pl-smi">x</span> <span class="pl-k">&lt;-</span> <span class="pl-k">matrix</span>(c(grepthis(<span class="pl-s"><span class="pl-pds">"</span>Acc<span class="pl-pds">"</span></span>), grepthis(<span class="pl-s"><span class="pl-pds">"</span>Gyro<span class="pl-pds">"</span></span>)), <span class="pl-v">ncol</span><span class="pl-k">=</span>nrow(<span class="pl-smi">y</span>))
<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">featInstrument</span> <span class="pl-k">&lt;-</span> <span class="pl-k">factor</span>(<span class="pl-smi">x</span> <span class="pl-k">%*%</span> <span class="pl-smi">y</span>, <span class="pl-v">labels</span><span class="pl-k">=</span>c(<span class="pl-s"><span class="pl-pds">"</span>Accelerometer<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>Gyroscope<span class="pl-pds">"</span></span>))
<span class="pl-smi">x</span> <span class="pl-k">&lt;-</span> <span class="pl-k">matrix</span>(c(grepthis(<span class="pl-s"><span class="pl-pds">"</span>BodyAcc<span class="pl-pds">"</span></span>), grepthis(<span class="pl-s"><span class="pl-pds">"</span>GravityAcc<span class="pl-pds">"</span></span>)), <span class="pl-v">ncol</span><span class="pl-k">=</span>nrow(<span class="pl-smi">y</span>))
<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">featAcceleration</span> <span class="pl-k">&lt;-</span> <span class="pl-k">factor</span>(<span class="pl-smi">x</span> <span class="pl-k">%*%</span> <span class="pl-smi">y</span>, <span class="pl-v">labels</span><span class="pl-k">=</span>c(<span class="pl-c1">NA</span>, <span class="pl-s"><span class="pl-pds">"</span>Body<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>Gravity<span class="pl-pds">"</span></span>))
<span class="pl-smi">x</span> <span class="pl-k">&lt;-</span> <span class="pl-k">matrix</span>(c(grepthis(<span class="pl-s"><span class="pl-pds">"</span>mean()<span class="pl-pds">"</span></span>), grepthis(<span class="pl-s"><span class="pl-pds">"</span>std()<span class="pl-pds">"</span></span>)), <span class="pl-v">ncol</span><span class="pl-k">=</span>nrow(<span class="pl-smi">y</span>))
<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">featVariable</span> <span class="pl-k">&lt;-</span> <span class="pl-k">factor</span>(<span class="pl-smi">x</span> <span class="pl-k">%*%</span> <span class="pl-smi">y</span>, <span class="pl-v">labels</span><span class="pl-k">=</span>c(<span class="pl-s"><span class="pl-pds">"</span>Mean<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>SD<span class="pl-pds">"</span></span>))
<span class="pl-c">## Features with 1 category</span>
<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">featJerk</span> <span class="pl-k">&lt;-</span> <span class="pl-k">factor</span>(grepthis(<span class="pl-s"><span class="pl-pds">"</span>Jerk<span class="pl-pds">"</span></span>), <span class="pl-v">labels</span><span class="pl-k">=</span>c(<span class="pl-c1">NA</span>, <span class="pl-s"><span class="pl-pds">"</span>Jerk<span class="pl-pds">"</span></span>))
<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">featMagnitude</span> <span class="pl-k">&lt;-</span> <span class="pl-k">factor</span>(grepthis(<span class="pl-s"><span class="pl-pds">"</span>Mag<span class="pl-pds">"</span></span>), <span class="pl-v">labels</span><span class="pl-k">=</span>c(<span class="pl-c1">NA</span>, <span class="pl-s"><span class="pl-pds">"</span>Magnitude<span class="pl-pds">"</span></span>))
<span class="pl-c">## Features with 3 categories</span>
<span class="pl-smi">n</span> <span class="pl-k">&lt;-</span> <span class="pl-c1">3</span>
<span class="pl-smi">y</span> <span class="pl-k">&lt;-</span> <span class="pl-k">matrix</span>(seq(<span class="pl-c1">1</span>, <span class="pl-smi">n</span>), <span class="pl-v">nrow</span><span class="pl-k">=</span><span class="pl-smi">n</span>)
<span class="pl-smi">x</span> <span class="pl-k">&lt;-</span> <span class="pl-k">matrix</span>(c(grepthis(<span class="pl-s"><span class="pl-pds">"</span>-X<span class="pl-pds">"</span></span>), grepthis(<span class="pl-s"><span class="pl-pds">"</span>-Y<span class="pl-pds">"</span></span>), grepthis(<span class="pl-s"><span class="pl-pds">"</span>-Z<span class="pl-pds">"</span></span>)), <span class="pl-v">ncol</span><span class="pl-k">=</span>nrow(<span class="pl-smi">y</span>))
<span class="pl-smi">dt</span><span class="pl-k">$</span><span class="pl-smi">featAxis</span> <span class="pl-k">&lt;-</span> <span class="pl-k">factor</span>(<span class="pl-smi">x</span> <span class="pl-k">%*%</span> <span class="pl-smi">y</span>, <span class="pl-v">labels</span><span class="pl-k">=</span>c(<span class="pl-c1">NA</span>, <span class="pl-s"><span class="pl-pds">"</span>X<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>Y<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>Z<span class="pl-pds">"</span></span>))</pre></div>

<p>Check to make sure all possible combinations of <code>feature</code> are accounted for by all possible combinations of the factor class variables.</p>

<div class="highlight highlight-source-r"><pre><span class="pl-smi">r1</span> <span class="pl-k">&lt;-</span> nrow(<span class="pl-smi">dt</span>[, .<span class="pl-smi">N</span>, <span class="pl-v">by</span><span class="pl-k">=</span>c(<span class="pl-s"><span class="pl-pds">"</span>feature<span class="pl-pds">"</span></span>)])
<span class="pl-smi">r2</span> <span class="pl-k">&lt;-</span> nrow(<span class="pl-smi">dt</span>[, .<span class="pl-smi">N</span>, <span class="pl-v">by</span><span class="pl-k">=</span>c(<span class="pl-s"><span class="pl-pds">"</span>featDomain<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>featAcceleration<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>featInstrument<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>featJerk<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>featMagnitude<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>featVariable<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>featAxis<span class="pl-pds">"</span></span>)])
<span class="pl-smi">r1</span> <span class="pl-k">==</span> <span class="pl-smi">r2</span></pre></div>

<p>Yes, I accounted for all possible combinations. <code>feature</code> is now redundant.</p>

<h2><a id="user-content-create-a-tidy-data-set" class="anchor" href="#create-a-tidy-data-set" aria-hidden="true"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M4 9h1v1h-1c-1.5 0-3-1.69-3-3.5s1.55-3.5 3-3.5h4c1.45 0 3 1.69 3 3.5 0 1.41-0.91 2.72-2 3.25v-1.16c0.58-0.45 1-1.27 1-2.09 0-1.28-1.02-2.5-2-2.5H4c-0.98 0-2 1.22-2 2.5s1 2.5 2 2.5z m9-3h-1v1h1c1 0 2 1.22 2 2.5s-1.02 2.5-2 2.5H9c-0.98 0-2-1.22-2-2.5 0-0.83 0.42-1.64 1-2.09v-1.16c-1.09 0.53-2 1.84-2 3.25 0 1.81 1.55 3.5 3 3.5h4c1.45 0 3-1.69 3-3.5s-1.5-3.5-3-3.5z"></path></svg></a>Create a tidy data set</h2>

<p>Create a data set with the average of each variable for each activity and each subject.</p>

<div class="highlight highlight-source-r"><pre>setkey(<span class="pl-smi">dt</span>, <span class="pl-smi">subject</span>, <span class="pl-smi">activity</span>, <span class="pl-smi">featDomain</span>, <span class="pl-smi">featAcceleration</span>, <span class="pl-smi">featInstrument</span>, <span class="pl-smi">featJerk</span>, <span class="pl-smi">featMagnitude</span>, <span class="pl-smi">featVariable</span>, <span class="pl-smi">featAxis</span>)
<span class="pl-smi">dtTidy</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">dt</span>[, <span class="pl-k">list</span>(<span class="pl-v">count</span> <span class="pl-k">=</span> .<span class="pl-smi">N</span>, <span class="pl-v">average</span> <span class="pl-k">=</span> mean(<span class="pl-smi">value</span>)), <span class="pl-v">by</span><span class="pl-k">=</span>key(<span class="pl-smi">dt</span>)]</pre></div>

<p>Make codebook.</p>

<div class="highlight highlight-source-r"><pre>knit(<span class="pl-s"><span class="pl-pds">"</span>makeCodebook.Rmd<span class="pl-pds">"</span></span>, <span class="pl-v">output</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">"</span>codebook.md<span class="pl-pds">"</span></span>, <span class="pl-v">encoding</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">"</span>ISO8859-1<span class="pl-pds">"</span></span>, <span class="pl-v">quiet</span><span class="pl-k">=</span><span class="pl-c1">TRUE</span>)
markdownToHTML(<span class="pl-s"><span class="pl-pds">"</span>codebook.md<span class="pl-pds">"</span></span>, <span class="pl-s"><span class="pl-pds">"</span>codebook.html<span class="pl-pds">"</span></span>)</pre></div>
</article>
  </div>

</div>

<button type="button" data-facebox="#jump-to-line" data-facebox-class="linejump" data-hotkey="l" class="hidden">Jump to Line</button>
<div id="jump-to-line" style="display:none">
  <!-- </textarea> --><!-- '"` --><form accept-charset="UTF-8" action="" class="js-jump-to-line-form" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
    <input class="form-control linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" aria-label="Jump to line" autofocus>
    <button type="submit" class="btn">Go</button>
</form></div>

  </div>
  <div class="modal-backdrop"></div>
</div>


    </div>
  </div>

    </div>

        <div class="container site-footer-container">
  <div class="site-footer" role="contentinfo">
    <ul class="site-footer-links right">
        <li><a href="https://status.github.com/" data-ga-click="Footer, go to status, text:status">Status</a></li>
      <li><a href="https://developer.github.com" data-ga-click="Footer, go to api, text:api">API</a></li>
      <li><a href="https://training.github.com" data-ga-click="Footer, go to training, text:training">Training</a></li>
      <li><a href="https://shop.github.com" data-ga-click="Footer, go to shop, text:shop">Shop</a></li>
        <li><a href="https://github.com/blog" data-ga-click="Footer, go to blog, text:blog">Blog</a></li>
        <li><a href="https://github.com/about" data-ga-click="Footer, go to about, text:about">About</a></li>

    </ul>

    <a href="https://github.com" aria-label="Homepage" class="site-footer-mark" title="GitHub">
      <svg aria-hidden="true" class="octicon octicon-mark-github" height="24" version="1.1" viewBox="0 0 16 16" width="24"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59 0.4 0.07 0.55-0.17 0.55-0.38 0-0.19-0.01-0.82-0.01-1.49-2.01 0.37-2.53-0.49-2.69-0.94-0.09-0.23-0.48-0.94-0.82-1.13-0.28-0.15-0.68-0.52-0.01-0.53 0.63-0.01 1.08 0.58 1.23 0.82 0.72 1.21 1.87 0.87 2.33 0.66 0.07-0.52 0.28-0.87 0.51-1.07-1.78-0.2-3.64-0.89-3.64-3.95 0-0.87 0.31-1.59 0.82-2.15-0.08-0.2-0.36-1.02 0.08-2.12 0 0 0.67-0.21 2.2 0.82 0.64-0.18 1.32-0.27 2-0.27 0.68 0 1.36 0.09 2 0.27 1.53-1.04 2.2-0.82 2.2-0.82 0.44 1.1 0.16 1.92 0.08 2.12 0.51 0.56 0.82 1.27 0.82 2.15 0 3.07-1.87 3.75-3.65 3.95 0.29 0.25 0.54 0.73 0.54 1.48 0 1.07-0.01 1.93-0.01 2.2 0 0.21 0.15 0.46 0.55 0.38C13.71 14.53 16 11.53 16 8 16 3.58 12.42 0 8 0z"></path></svg>
</a>
    <ul class="site-footer-links">
      <li>&copy; 2016 <span title="0.04617s from github-fe150-cp1-prd.iad.github.net">GitHub</span>, Inc.</li>
        <li><a href="https://github.com/site/terms" data-ga-click="Footer, go to terms, text:terms">Terms</a></li>
        <li><a href="https://github.com/site/privacy" data-ga-click="Footer, go to privacy, text:privacy">Privacy</a></li>
        <li><a href="https://github.com/security" data-ga-click="Footer, go to security, text:security">Security</a></li>
        <li><a href="https://github.com/contact" data-ga-click="Footer, go to contact, text:contact">Contact</a></li>
        <li><a href="https://help.github.com" data-ga-click="Footer, go to help, text:help">Help</a></li>
    </ul>
  </div>
</div>



    
    

    <div id="ajax-error-message" class="ajax-error-message flash flash-error">
      <svg aria-hidden="true" class="octicon octicon-alert" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M15.72 12.5l-6.85-11.98C8.69 0.21 8.36 0.02 8 0.02s-0.69 0.19-0.87 0.5l-6.85 11.98c-0.18 0.31-0.18 0.69 0 1C0.47 13.81 0.8 14 1.15 14h13.7c0.36 0 0.69-0.19 0.86-0.5S15.89 12.81 15.72 12.5zM9 12H7V10h2V12zM9 9H7V5h2V9z"></path></svg>
      <button type="button" class="flash-close js-flash-close js-ajax-error-dismiss" aria-label="Dismiss error">
        <svg aria-hidden="true" class="octicon octicon-x" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path d="M7.48 8l3.75 3.75-1.48 1.48-3.75-3.75-3.75 3.75-1.48-1.48 3.75-3.75L0.77 4.25l1.48-1.48 3.75 3.75 3.75-3.75 1.48 1.48-3.75 3.75z"></path></svg>
      </button>
      Something went wrong with that request. Please try again.
    </div>


      
      <script crossorigin="anonymous" integrity="sha256-4uyi3wBCkx9VD1mDG11JLFwnloJ5fDExqZzUPD8JF9M=" src="https://assets-cdn.github.com/assets/frameworks-e2eca2df0042931f550f59831b5d492c5c279682797c3131a99cd43c3f0917d3.js"></script>
      <script async="async" crossorigin="anonymous" integrity="sha256-LkOzPIQQcypie7rwV1frNYTsnPeodH9GzNIzaigiP2A=" src="https://assets-cdn.github.com/assets/github-2e43b33c8410732a627bbaf05757eb3584ec9cf7a8747f46ccd2336a28223f60.js"></script>
      
      
      
      
      
      
    <div class="js-stale-session-flash stale-session-flash flash flash-warn flash-banner hidden">
      <svg aria-hidden="true" class="octicon octicon-alert" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path d="M15.72 12.5l-6.85-11.98C8.69 0.21 8.36 0.02 8 0.02s-0.69 0.19-0.87 0.5l-6.85 11.98c-0.18 0.31-0.18 0.69 0 1C0.47 13.81 0.8 14 1.15 14h13.7c0.36 0 0.69-0.19 0.86-0.5S15.89 12.81 15.72 12.5zM9 12H7V10h2V12zM9 9H7V5h2V9z"></path></svg>
      <span class="signed-in-tab-flash">You signed in with another tab or window. <a href="">Reload</a> to refresh your session.</span>
      <span class="signed-out-tab-flash">You signed out in another tab or window. <a href="">Reload</a> to refresh your session.</span>
    </div>
    <div class="facebox" id="facebox" style="display:none;">
  <div class="facebox-popup">
    <div class="facebox-content" role="dialog" aria-labelledby="facebox-header" aria-describedby="facebox-description">
    </div>
    <button type="button" class="facebox-close js-facebox-close" aria-label="Close modal">
      <svg aria-hidden="true" class="octicon octicon-x" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path d="M7.48 8l3.75 3.75-1.48 1.48-3.75-3.75-3.75 3.75-1.48-1.48 3.75-3.75L0.77 4.25l1.48-1.48 3.75 3.75 3.75-3.75 1.48 1.48-3.75 3.75z"></path></svg>
    </button>
  </div>
</div>

  </body>
</html>

