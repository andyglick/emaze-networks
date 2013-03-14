package net.emaze.networks;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.tuples.Pair;

public class SubtractIpFromNetwork implements BinaryDelegate<Set<Network>, Network, Ip> {

    @Override
    public Set<Network> perform(Network minuend, Ip subtrahend) {
        return recursivelySubtract(minuend, subtrahend);
    }

    private Set<Network> recursivelySubtract(Network minuend, Ip subtrahend) {
        if (!minuend.contains(subtrahend)) {
            return Collections.singleton(minuend);
        }
        if (minuend.netmask().equals(Mask.NARROWEST)) {
            return Collections.emptySet();
        }
        final Set<Network> reminder = new HashSet<>();
        final Pair<Network, Network> split = minuend.split();
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
