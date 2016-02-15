newLISP base for different stuff

1. master: stable
   b) m_fix_.* : temp branch from master

3. devel (development)
   - if rel_* overkill:
     - rebase on master,
     - change version number,
     - ff merge into master
   * d_fix_.* : temp branch from devel
   * d_.* (devel new feature) : rebased on devel

3. rel_* (release)
   - change version number
   - rebase onto master
   - ff merge into master gives new release
   
4. exp[_.]+ (experimental):
   - rebase on devel|dfix
   - ff merge into devel


Link:
  http://nvie.com/posts/a-successful-git-branching-model/
