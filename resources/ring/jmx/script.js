document.addEventListener('DOMContentLoaded', function() {
    document.querySelectorAll('article.operation').forEach(function updateArticle(article) {
      var e = article.querySelector('form');
      e.addEventListener('submit', function (event) {
        var data = this;
        article.classList.add('loading');
        fetch(window.location.href, {
          method: 'POST',
          body: new FormData(data)
        }).then(res=>res.text())
          .then(function (data) {
            article.classList.remove('loading');
            var t = document.createElement('template');
            t.innerHTML = data;
            var parent = article.parentNode;
            article.replaceWith(t.content);
            updateArticle(parent.querySelector('article.operation'));
        });
        event.preventDefault();
      });
    });
  });
