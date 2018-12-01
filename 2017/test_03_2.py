import pytest
from aoc_03_spiral2 import (
    Point, PtArray, puzzle_input,
)


@pytest.fixture(params=[[0, 0, 1], [1, 0, 1], [1, 1, 2], [0, 1, 4], [-1, 1, 5], [-1, 0, 10]])
def before(request):
    empty_array = [
        [
            [Point(x, y), 0] for x in range(PtArray.min_x, PtArray.max_x + 1)
        ] for y in range(PtArray.min_y, PtArray.max_y + 1)
    ]
    st1 = PtArray(empty_array)
    st1.fill_store(puzzle_input)
    return request.param, st1

def test_fill_store(before):
    (params, st1) = before
    x, y, expected_sum = params[0], params[1], params[2]
    print('st1.get_val: %s' % st1.get_val(0, 0))
    actual_sum = st1.get_val(x, y)
    assert actual_sum == expected_sum, 'actual sum = ' + str(actual_sum)
