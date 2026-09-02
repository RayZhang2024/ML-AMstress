import ast
import pathlib
import unittest


SOURCE = pathlib.Path(__file__).resolve().parents[1] / 'create_input.py'
TREE = ast.parse(SOURCE.read_text())
RESOLVER_NODE = next(node for node in TREE.body
                     if isinstance(node, ast.FunctionDef)
                     and node.name == '_resolve_heat_treatment_step')
NAMESPACE = {'CREATED': 'CREATED'}
exec(compile(ast.Module(body=[RESOLVER_NODE], type_ignores=[]),
             str(SOURCE), 'exec'), NAMESPACE, NAMESPACE)
resolve_heat_treatment_step = NAMESPACE['_resolve_heat_treatment_step']


class MockStep(object):
    def __init__(self, period):
        self.timePeriod = period


class MockInteraction(object):
    def __init__(self, created_index, final_index):
        self.history = (['NOT_YET_ACTIVE'] * created_index +
                        ['CREATED'] +
                        ['PROPAGATED'] * (final_index - created_index))


class MockModel(object):
    def __init__(self, prefix, final_index, removals, period=1.0):
        self.steps = {'Initial': MockStep(0.0)}
        for index in range(1, final_index + 1):
            self.steps['%s-%d' % (prefix, index)] = MockStep(period)
        self.interactions = {}
        for name, created_index in removals.items():
            self.interactions[name] = MockInteraction(created_index, final_index)


class HeatTreatmentStepTests(unittest.TestCase):
    def test_normal_step_uses_explicit_heat_treatment_period(self):
        model = MockModel('Step', 6, {}, period=1.0)
        model.steps['Step-6'].timePeriod = 0.05
        self.assertEqual(resolve_heat_treatment_step(model, 2), (6, None))

    def test_bstep_uses_modelchange_removal_history(self):
        model = MockModel('BStep', 8, {
            'Int-4': 5,
            'Int-bottom-1': 6,
            'Int-bottom-2': 7,
        })
        self.assertEqual(resolve_heat_treatment_step(model, 2), (8, None))

    def test_missing_distinct_post_removal_step_fails(self):
        model = MockModel('BStep', 7, {
            'Int-4': 5,
            'Int-bottom-1': 6,
            'Int-bottom-2': 7,
        })
        step, error = resolve_heat_treatment_step(model, 2)
        self.assertIsNone(step)
        self.assertIn('no distinct post-removal', error)


if __name__ == '__main__':
    unittest.main()
