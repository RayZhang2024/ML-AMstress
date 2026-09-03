# -*- coding: utf-8 -*-
"""Reusable, optional GUI-side visualization panel.

The panel deliberately knows nothing about Abaqus models.  When the optional
PyVista/PyVistaQt stack is available it hosts an interactive ``QtInteractor``;
otherwise it remains a usable empty-state widget so non-visual workflows can
continue to run.
"""

from PyQt5 import QtCore, QtWidgets

try:
    import pyvista as _pv
    from pyvistaqt import QtInteractor as _QtInteractor
except Exception:
    _pv = None
    _QtInteractor = None


class VisualizationPanel(QtWidgets.QWidget):
    """Embeddable viewer with clear/reset and safe content replacement."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self._plotter = None
        self._empty_label = QtWidgets.QLabel()
        self._empty_label.setAlignment(QtCore.Qt.AlignCenter)
        self._empty_label.setWordWrap(True)

        self._stack = QtWidgets.QStackedLayout()
        self._stack.addWidget(self._empty_label)

        if _pv is not None and _QtInteractor is not None:
            try:
                self._plotter = _QtInteractor(self)
                self._stack.addWidget(self._plotter)
            except Exception:
                # Qt/VTK initialization can fail in headless or partially
                # configured environments; retain the graceful empty state.
                self._plotter = None

        layout = QtWidgets.QVBoxLayout(self)
        layout.addLayout(self._stack)
        self.clear()

    @property
    def backend_available(self):
        """Whether an interactive PyVista renderer was initialized."""
        return self._plotter is not None

    def _set_empty(self, message=None):
        if message is None:
            if self.backend_available:
                message = "No visualization loaded."
            else:
                message = (
                    "Visualization support is unavailable. Install the optional "
                    "PyVista/PyVistaQt/VTK dependencies to enable the viewer."
                )
        self._empty_label.setText(message)
        self._stack.setCurrentWidget(self._empty_label)

    def clear(self):
        """Remove all actors and return the panel to its empty state."""
        if self._plotter is not None:
            try:
                self._plotter.clear()
            except Exception:
                # Clearing is best effort; the empty state must remain usable.
                pass
        self._set_empty()

    def reset_camera(self):
        """Reset the interactive camera when a renderer is available."""
        if self._plotter is None:
            return
        try:
            self._plotter.reset_camera()
            self._plotter.render()
        except Exception:
            pass

    def show_points(self, points):
        """Display an ``(N, 3)`` GUI-side point collection.

        The method replaces existing actors atomically from the caller's point
        of view: stale content is cleared before the new dataset is added.  It
        returns ``True`` only when the dataset was displayed by PyVista.
        """
        if self._plotter is None or _pv is None:
            self._set_empty()
            return False
        try:
            mesh = _pv.PolyData(points)
            if mesh.n_points == 0:
                self.clear()
                return False
            self._plotter.clear()
            self._plotter.add_mesh(
                mesh,
                color="#4c78a8",
                point_size=8,
                render_points_as_spheres=True,
            )
            self._plotter.reset_camera()
            self._plotter.render()
            self._stack.setCurrentWidget(self._plotter)
            return True
        except Exception as exc:
            self._set_empty("Visualization could not be loaded: %s" % exc)
            return False
