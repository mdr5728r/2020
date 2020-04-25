# [The Ultimate Markdown Guide (for Jupyter Notebook)](https://medium.com/analytics-vidhya/the-ultimate-markdown-guide-for-jupyter-notebook-d5e5abf728fd)
>Hannan Satopay


## <br>**Markdown Syntax**<br> Collections

#### HEADINGS

```
<br>**Markdown Syntax**<br>
# Header 1
## Header 2
### Header 3
#### Header 4
##### Header 5
###### Header 6
```

### Equivalent HTML Syntax
```
<h1>Header 1</h1>
<h2>Header 2</h2>
<h3>Header 3</h3>
<h4>Header 4</h4>
<h5>Header 5</h5>
<h6>Header 6</h6>
```
<br> **Rendered Output**

# Header 1
## Header 2
### Header 3
#### Header 4
##### Header 5
###### Header 6


#### TEXT EMPHASIS
BOLD
<br>**Markdown Syntax**<br>
**This is bold text**  
__This is bold text__
Equivalent HTML Syntax
<strong>This is bold text</strong>
<br> **Rendered Output**

ITALIC
<br>**Markdown Syntax**<br>
*This is italic text*  
_This is italic text_
Equivalent HTML Syntax
<em>This is italic text</em>
<br> **Rendered Output**

BOLD & ITALIC
<br>**Markdown Syntax**<br>
***Bold and Italic***  
___Bold and Italic___
Equivalent HTML Syntax
<strong><em> Bold and Italic </em><strong>
<br> **Rendered Output**

STRIKETHROUGH
<br>**Markdown Syntax**<br>
~~Scratch this~~
Equivalent HTML Syntax
<del>Scratch this</del>
<br> **Rendered Output**

#### BACKSLASH ESCAPE
Backslash Escape prevents Markdown from interpreting a character as an instruction, rather than as the character itself.
<br>**Markdown Syntax**<br>
\# Not a header
Equivalent HTML Syntax
# Not a header
<br> **Rendered Output**

####  PARAGRAPHS
A paragraph is simply one or more consecutive lines of text, separated by one or more blank lines.
<br>**Markdown Syntax**<br>
Hello World!
We are learning Markdown.
Equivalent HTML Syntax
<p>Hello World!</p>
<p>We are learning Markdown.</p>

#### LINE BREAK
A line break is the termination of the previous line and the beginning of a new line.
<br>**Markdown Syntax**<br>
To force a line return, place two or more empty spaces at the end of a line and press the Enter key.
Some text  
Some more text
Equivalent HTML Syntax
The line break tag starts with <br> tag with no closing tag which breaks the line, and the remaining contents begin with a new line.
Some text <br>
Some more text
<br> **Rendered Output**

#### BLOCKQUOTES
Blockquotes can hold the large chunk of text and are generally indented.
<br>**Markdown Syntax**<br>
> This is a blockquote
Equivalent HTML Syntax
<blockquote>This is a blockquote</blockquote>
    
<br> **Rendered Output**

#### NESTED BLOCK QUOTING
<br>**Markdown Syntax**<br>
> some text
>> and then some more text
>>> and then some more
Equivalent HTML Syntax
<blockquote>
<p>some text</p>
<blockquote>
<p>and then some more text</p>
<blockquote>
<p>and then some more</p>
</blockquote>
</blockquote>
</blockquote>
<br> **Rendered Output**

#### HORIZONTAL LINE
<br>**Markdown Syntax**<br>
---
___
***
Equivalent HTML Syntax
<hr>
<br> **Rendered Output**

#### ORDERED LIST
The Ordered List is a numbered list.
<br>**Markdown Syntax**<br>
1. Cheese
2. Carrot
3. Coconut
Note: Numbering is irrelevant
Equivalent HTML Syntax
<ol>
 <li>Cheese</li>
 <li>Carrot</li>
 <li>Coconut</li>
</ol>
<br> **Rendered Output**

#### UNORDERED LIST
The Unordered list is a bullet list.
<br>**Markdown Syntax**<br>
- Cheese
- Carrot
- Coconut
Equivalent HTML Syntax
<ul>
 <li>Cheese</li>
 <li>Carrot</li>
 <li>Coconut</li>
 </ul>
<br> **Rendered Output**

#### GRAPHICS
You can attach graphics (such as images) to a notebook in Markdown cells.
Note1: You can also Drag and Drop your images to the Markdown cell to attach it to the notebook.
Note2: Below I have used links to images on the web but you can very well use an offline image by adding the complete filename (plus the file path if it is in a different directory other then the Jupyter Notebook).
<br>**Markdown Syntax**<br>
One simple way of adding an image to a Markdown cell is through the following syntax:
![](https://www.python.org/static/community_logos/python-logo-master-v3-TM.png)
If you want to add a hover title to the image then you can simply modify the syntax like below:
![](https://www.python.org/static/community_logos/python-logo-master-v3-TM.png “Python Logo”)
You can also use the reference-style format for the images:
![][some-id]

[some-id]: https://www.python.org/static/community_logos/python-logo-master-v3-TM.png "Python Logo"
Equivalent HTML Syntax
<img src="https://www.python.org/static/community_logos/python-logo-master-v3-TM.png" title="Python Logo"/>
<br> **Rendered Output**

#### HYPERLINKS
##### AUTOMATIC LINKS
<br>**Markdown Syntax**<br>
https://en.wikipedia.org
Equivalent HTML Syntax
<a href="https://en.wikipedia.org">https://en.wikipedia.org</a>

##### STANDARD LINKS
<br>**Markdown Syntax**<br>
[click me](https://en.wikipedia.org)
Equivalent HTML Syntax
<a href="https://en.wikipedia.org">click me</a>
##### STANDARD LINKS (WITH MOUSE-OVER TITLES)
<br>**Markdown Syntax**<br>
[click me](https://en.wikipedia.org "Wikipedia")
Equivalent HTML Syntax
<a href="https://en.wikipedia.org" title=”Wikipedia”>click me</a>
    
##### REFERENCE-STYLE LINKS
<br>**Markdown Syntax**<br>
This is [a reference][id]
[id]: https://en.wikipedia.org "Wikipedia"
Note: Link IDs are not case-sensitive.
You don’t really have to give your link an ID. If you use the words in the first set of brackets to later define the link, Markdown will understand it. This works as follows:
This is [a reference][]
[a reference]: https://en.wikipedia.org "Wikipedia"
Equivalent HTML Syntax
This is <a href="https://en.wikipedia.org" title="Wikipedia">a reference</a>
<br> **Rendered Output**

##### ANCHOR LINKS
An anchor link is a link on a page that brings you to a specific place on that page. In a Jupyter Notebook, it can be used to link to any section of the notebook for easy navigation.
Note: The ID used for the linking should be unique to avoid misdirection.
###### Technique 1
Create a new cell above the section you want to link to and add the following line of code:
<a id="id"></a>
To create an anchor link that links to the above section, simply add the following line of code:
[Section title](#id)
###### Technique 2
If you have a section with the heading — My Great Heading then to add an anchor link to this section, simply add a hyphen in place of the blank space like below:
[Section title](#My-Great-Heading)
<br> **Rendered Output**

#### TASK LISTS
<br>**Markdown Syntax**<br>
- [x] Some task
- [ ] Some more task
Equivalent HTML Syntax
<input type=”checkbox” disabled checked> Some task
<input type=”checkbox” disabled> Some more task
<br> **Rendered Output**

#### CODE SECTION
<br>**Markdown Syntax**<br>
Inline Code:  
    
`some piece of inline code`
```
some piece of block code
```
You can also perform syntax highlighting like below:

    javascript
```javascript
var s = "JavaScript syntax highlighting";
alert(s);
```
    Python
```python
s = "Python syntax highlighting"
print s
```
##### Equivalent HTML Syntax
You can use the `<code>` tags in HTML to get similar results. For syntax highlighting, appropriate classes have to be utilized for the different elements.
<br> **Rendered Output**

#### TABLE
A table can be constructed using | (pipe symbol) and — (dash) to mark columns and rows.
<br>**Markdown Syntax**<br>
    
|Header|Header|Header|
|------|------|------|
|A     |B     |C     |
|X     |Y     |Z     |

    Note: It is not important to add spaces after each text, roughly aligning with the columns will do just fine. Also, the number of dashes is irrelevant and is just cosmetical.

The text in each header and cell of a table will by default justify to the right.

For manually changing the justification, you can use:
* :-: for centered
* — for right centered
* :- for left centered

Equivalent HTML Syntax
```
<table>
<thead>
<tr><th>Header</th><th>Header</th><th>Header</th></tr>
</thead>
<tbody>
<tr><td>A</td><td>B</td><td>C</td></tr>
<tr><td>X</td><td>Y</td><td>Z</td></tr>
</tbody>
</table>  
```    
<br> **Rendered Output**
<table>
<thead>
<tr><th>Header</th><th>Header</th><th>Header</th></tr>
</thead>
<tbody>
<tr><td>A</td><td>B</td><td>C</td></tr>
<tr><td>X</td><td>Y</td><td>Z</td></tr>
</tbody>
</table>  
    
    
#### INLINE HTML
##### TEXT COLOR
`<span style="color:color">Text</span>`
    
Where color = blue|red|green|pink|yellow
For a list of all the supported color names, checkout [HTML Color Names](https://www.w3schools.com/colors/colors_names.asp).
You can use also use the HEX color codes for customizing the text color.
<br> **Rendered Output**  
<span style="color:blue">Blue Text</span> | <span style="color:red">Blue Text</span> | <span style="color:green">Blue Text</span> | <span style="color:pink">Blue Text</span> | <span style="color:olive">Blue Text</span>
##### TEXT FONT FAMILY
`<span style="font-family:Comic Sans MS">This is a Comic Sans MS font text</span>`

For a list of some commonly used fonts, checkout [CSS Font Family List](https://www.tutorialbrain.com/css_tutorial/css_font_family_list/).
<br> **Rendered Output**  
    <center><span style="font-family:Comic Sans MS">This is a Comic Sans MS font centered text. </span></center>
##### COLORED NOTE BOXES
Use one of the following <div> tags to display text in a colored box. The color of the box is determined by the alert type that is specified.

Blue boxes (alert-info)
<div class="alert alert-block alert-info">
<b>Tip:</b> Use blue boxes (alert-info) for tips and notes.</div>
Yellow boxes (alert-warning)
<div class="alert alert-block alert-warning">
<b>Example:</b> Use yellow boxes for examples that are not inside code cells, or use for mathematical formulas if needed. Typically also used to display warning messages.
</div>
Green boxes (alert-success)
<div class="alert alert-block alert-success">
<b>Success:</b> This alert box indicates a successful or positive action.
</div>
Red boxes (alert-danger)
<div class="alert alert-block alert-danger">
<b>Danger:</b> This alert box indicates a dangerous or potentially negative action.
</div>

#### CELL BACKGROUND COLOR
`<code style="background:yellow;color:black">Useful for highlighting to grab the attention of the reader towards certain points.</code>` 
<br> **Rendered Output**  
<code style="background:yellow;color:black">Useful for highlighting to grab the attention of the reader towards certain points.</code>
    
I also tend to use the following color style when adding a piece of terminal code to a Markdown cell:
```html
<p style="background:black">
<code style="background:black;color:white">C:\Users\YOUR_USERNAME> pip3 install roughviz
</code>
</p>
```
**Rendered Output** 
<p style="background:black">
<code style="background:black;color:white">C:\Users\YOUR_USERNAME> pip3 install roughviz
</code>
</p>
By the way, roughviz is a Python visualization library that I have created for creating sketchy/hand-drawn styled charts. Do check it out on Github and PyPI.


#### HTML MARK TAG
Highlight parts of a text:  
    Do not forget to buy `<mark>milk</mark>` today.
<br>
**Rendered Output**  
    Do not forget to buy <mark>milk</mark> today.

#### DEFINITION LISTS
```html
<dl>
<dt>First Term</dt>
<dd>This is the definition of the first term.</dd>
<dt>Second Term</dt>
<dd>This is one definition of the second term. </dd>
<dd>This is another definition of the second term.</dd>
</dl>
```
**Rendered Output**
<dl>
<dt>First Term</dt>
<dd>This is the definition of the first term.</dd>
<dt>Second Term</dt>
<dd>This is one definition of the second term. </dd>
<dd>This is another definition of the second term.</dd>
</dl>

#### NAVIGATION MENU
It defines a set of navigation links.
```html
<nav>
<a href=”https://www.google.com"> LinkedIn</a> |
<a href=”/css/”>Github</a> |
<a href=”/js/”>Medium</a> |
</nav>
```
**Rendered Output**
<nav>
<a href=”https://www.google.com">LinkedIn</a> |
<a href=”/css/”>Github</a> |
<a href=”/js/”>Medium</a> |
</nav>
                                
#### LaTeX MATH
Jupyter Notebooks’ Markdown cells support LateX for formatting mathematical equations. To tell Markdown to interpret your text as LaTex, surround your input with dollar signs like this:

`$$\sqrt{k}$$`
    
**Rendered Output**  
$$\sqrt{k}$$
<br>
    
#### GEOMETRIC SHAPES
Use this code with a decimal or hex reference number from here: [UTF-8 Geometric shapes](http://www.w3schools.com/charsets/ref_utf_geometric.asp)
&#reference_number;
    
Decimal `&#9661;` = &#9661;<br>
Decimal `&#9717;` =  &#9717;<br>
Hexadecimal `&#x25BA;` = &#x25BA; 