---
---

@createComment = (comment) ->

    date = moment(comment.created_at)
    displayDate = formatDate(date)
    dateTooltip = date.toString()

    """
        <article class="comment">
            <header>
                <a href="#{comment.user.html_url}">
                    <img class="avatar" alt="@#{comment.user.login}" src="#{comment.user.avatar_url}"> 
                </a> 
                <a class="author" href="#{comment.user.html_url}">
                    #{comment.user.login}
                </a> 
                <a class="timestamp" href="#{comment.html_url}">
                    <time datetime="#{comment.created_at}" is="relative-time" title="#{dateTooltip}">#{displayDate}</time> 
                </a>  
            </header>
            <section class="comment-content">  
                #{comment.body_html}
            </section> 
        </article>
    """


formatDate = (date) ->
    now = moment()

    if now.diff(date, 'days') < 24
        date.fromNow()
    else if now.year() == date.year()
        date.format("D MMM")
    else date.format("D MMM YYYY")

