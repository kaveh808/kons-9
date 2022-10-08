(in-package #:kons-9)

#|
Greetings and welcome to kons-9!

These demos assume that you have succeeded in loading the system and opening
the graphics window. If you have not, please check the README file.

The demos below will walk you through some of the features of the software. You
will evaluate the expressions in each demo and the graphics in the window will
update accordingly.

We also assume that you are familiar with the basics of 3D graphics.

As of this writing, kons-9 runs on MacOS (Intel & M1) and Linux.

We hope you find the system enjoyable and useful.
|#

#|
(Demo 01 UI) status bar ========================================================

The status bar at the bottom of the window displays two lines of information. The
first line is information about the scene, and the second line shows the current
mouse and key bindings.

Evaluate the code below. This animation translates the shape in the X axis
direction.

To play animation, hold down SPACE key. Notice the frame counter updating in the
status bar. By default the scene starts at frame 0 and ends at frame 240.

Press '[' key to initialize the scene back to frame 0.
|#
(with-clear-scene
  (let ((shape (add-shape *scene* (make-cut-cube 2.0))))
    (add-motion *scene*
                (make-instance 'animator
                               :setup-fn (lambda () (translate-to shape (p! 0.0 0 0)))
                               :update-fn (lambda () (translate-by shape (p! 0.1 0 0)))))))

#|
(Demo 02 UI) menus =============================================================

The status bar indicates that the TAB key shows and hides menus.

Menus in kons-9 appear at the top left of the window. Press TAB to show the main
kons-9 menu.

Menus consist of a title in gray and a number of menu items. Menu items
highlight when moused over.  Each menu item has a keyboard shortcut which
triggers it. The keyboard shortcuts are simple key presses, not requiring any
modifier (e.g. COMMAND or SHIFT) keys.

Click on the "Display Menu" (or press the 'd' key).

The "Display" menu appears. To go back to the previous menu, press the
LEFT-ARROW key. Then press 'd' again to return to the "Display" menu.

If necessary, evaluate the code below (from Demo 01) so there is some geometry in
the scene.

Now try out the various menu items by clicking on them or using the keyboard
shortcuts. Note that the menu keyboard shortcuts are only active when the
menu is visible. A given key will have different effects when different
menus are active.

As before, play the animation using the SPACE key, and initialize the scene
using the '[' key. Note that the SPACE and '[' keys are examples of global key
bindings. They are always available no matter what menu is currently active.

You can click and navigate in the scene as usual when a menu is visble.
|#
(with-clear-scene
  (let ((shape (add-shape *scene* (make-cut-cube 2.0))))
    (add-motion *scene*
                (make-instance 'animator
                               :setup-fn (lambda () (translate-to shape (p! 0.0 0 0)))
                               :update-fn (lambda () (translate-by shape (p! 0.1 0 0)))))))

#|
(Demo 03 UI) inspectors ========================================================

[NOTE: As of this writing inspectors do not automatically update to reflect
changes in the scene.]

Continuing from the previous demo:

Press the left arrow key to go back to the kons-9 menu.

Click "Scene Menu" or press 's'.

Click "New Scene" or press 'n'.

The existing scene is cleared.

(To exit the system at any time, click on "Quit Scene" or press 'q'.)

Do the following:

Left arrow.

"Create Menu"

"Create Curve Menu"

"Create Circle Curve"

Note that you can do the above three steps by pressing 'c' three times. This
is an example of key shortcuts changing based on the current menu.

Left arrow.

"Create Polyhedron Menu"

"Create Octahendron"

Press TAB to hide the menu.

Press TAB again to show kons-9 menu.

"Inspect Menu"

"Shapes"

A shape inspector appears at the top right of the window. It shows the root
node of the scene shape hierarchy, called SHAPES.

Click on SHAPES. The node is selected.

Click on SHAPES again. The node is deselected.

Alt-click on SHAPES. The inspector expands to show the child shapes.

Click on the inspector entries to select and deselect the shapes.

Press ESCAPE to hide the inspector.

Press TAB to hide the menu.
|#

#|
(Demo 04 UI) multiple inspectors ===============================================

Press TAB, S, N to trigger "New Scene".

Press TAB to hide the menu.

Evaluate the code below.

Hold down space key to play animation. Press '[' key to go back to frame 0.

Press TAB, I, S, M.

Two inspectors appear: one for the scene shapes and one for the scene motions.

Alt-click on the entries to expand them.

Use the UP and DOWN ARROW keys to scroll the inspectors.

Press SHIFT left-arrow to hide the last inspector.

Press SHIFT left-arrow again to hide the remaining inspector.

Press I to show the Lisp data inspector. This is base on the SBCL "inspect"
utility. It will inspect the current selection or the scene if there is nothing
selected.

Click on various entries to display more inspectors.

Press SHIFT left-arrow to hide inspectors.

Press ESCAPE to hide all inspectors.
|#
(with-clear-scene
  (let ((group (add-shape *scene* (scatter-shapes-in-group
                                   (lambda () (make-cube 0.5))
                                   (make-grid-points 3 3 3 (p! -2 -2 -2) (p! 2 2 2))))))
    (map-hierarchy group
      (lambda (shape)
        (add-motion *scene*
                      (make-instance 'shape-animator
                                     :shape shape
                                     :setup-fn (lambda (anim)
                                                (rotate-to (shape anim) (p! 0 0 0)))
                                     :update-fn (lambda (anim)
                                                  (rotate-by (shape anim) (anim-data anim :rotate)))
                                     :data `((:rotate . ,(p! 0 (rand1 10) 0))))))
      :test #'is-leaf?)))

#|
END ============================================================================
|#
