package net.emaze.networks.my;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.tuples.Pair;


public class SubtractIpFromNetwork implements BinaryDelegate<Set<MyNetwork>, MyNetwork, MyIp> {

    @Override
    public Set<MyNetwork> perform(MyNetwork minuend, MyIp subtrahend) {
        return recursivelySubtract(minuend, subtrahend);
    }

    private Set<MyNetwork> recursivelySubtract(MyNetwork minuend, MyIp subtrahend) {
        if (!minuend.contains(subtrahend)) {
            return Collections.singleton(minuend);
        }
        if (minuend.netmask().isNarrowest()) {
            return Collections.emptySet();
        }
        final Set<MyNetwork> reminder = new HashSet<>();
        final Pair<MyNetwork, MyNetwork> split = minuend.split();
        if (split.first().contains(subtrahend)) {
            reminder.add(split.second());
            reminder.addAll(recursivelySubtract(split.first(), subtrahend));
        } else {
            reminder.add(split.first());
            reminder.addAll(recursivelySubtract(split.second(), subtrahend));
        }
        return reminder;
    }

}
