package net.emaze.networks;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.tuples.Pair;

public class SubtractIpv4FromNetwork implements BinaryDelegate<Set<Ipv4Network>, Ipv4Network, Ipv4> {

    @Override
    public Set<Ipv4Network> perform(Ipv4Network minuend, Ipv4 subtrahend) {
        return recursivelySubtract(minuend, subtrahend);
    }

    private Set<Ipv4Network> recursivelySubtract(Ipv4Network minuend, Ipv4 subtrahend) {
        if (!minuend.contains(subtrahend)) {
            return Collections.singleton(minuend);
        }
        if (minuend.netmask().equals(Ipv4Mask.NARROWEST)) {
            return Collections.emptySet();
        }
        final Set<Ipv4Network> reminder = new HashSet<>();
        final Pair<Ipv4Network, Ipv4Network> split = minuend.split();
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
