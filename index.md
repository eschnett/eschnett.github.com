---
layout: page
title: "Erik Schnetter's Blog"
tagline: Relativistic astrophysics, high performance computing, and software that connects them
---
{% include JB/setup %}

Physics is a science built upon distilling simple, universal
statements about a complex, messy world. I am a physicist. I use
computers.

## Posts

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>
