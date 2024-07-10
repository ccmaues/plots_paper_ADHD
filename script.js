document.addEventListener('DOMContentLoaded', function() {
  var codeBlocks = document.querySelectorAll('.code-hide');
  codeBlocks.forEach(function(block) {
    var button = block.querySelector('button');
    button.addEventListener('click', function() {
      var code = block.querySelector('.code');
      if (code.style.display === 'none') {
        code.style.display = 'block';
        button.textContent = 'Hide Code';
      } else {
        code.style.display = 'none';
        button.textContent = 'Show Code';
      }
    });
  });
});
