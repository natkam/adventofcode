import pytest
from aoc_03_spiral1 import calculate_distance

@pytest.fixture(params=[[1, 0], [9, 2], [12, 3], [23, 2], [1024, 31], [325489, 552]])
def before(request):
    return request.param

def test_calculate_distance(before):
    (number, expected_distance) = before
    actual_distance = calculate_distance(number)
    assert actual_distance == expected_distance, 'actual distance = ' + str(actual_distance)

# test_calculate_distance(1, 0)
# test_calculate_distance(12, 3)
# test_calculate_distance(23, 2)
# test_calculate_distance(1024, 31)
# test_calculate_distance(325489, 552)
