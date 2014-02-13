Obraz.el intends to facilitate use of Obraz static site
generator (http://obraz.pirx.ru/), which is mostly compatible
with Jekyll.  Hopefully, this package is usable with Jekyll
(https://github.com/mojombo/jekyll) too.

The package provides several user-visible commands:

  * `obraz-list-posts` shows a list of written posts, and allows to
    reopen the corresponding files in `_posts/` or start writing a new
    post.

    Default keybindings for the list of posts are:

        | Key | Command            |
        |-----+--------------------|
        | n   | next post          |
        | p   | previous post      |
        | r   | refresh            |
        | b   | build site         |
        | B   | serve site locally |

  * `obraz-new-post` helps to create a new text file in `_posts/` with
    a proper file name derived from the current date, and post title.

  * `obraz-build` and `obraz-serve` build and serve current site
    respectively.

Use `M-x customize-group RET obraz RET` to set the script location
and the post template.
