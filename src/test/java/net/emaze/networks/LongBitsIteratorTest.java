package net.emaze.networks;

import java.util.Iterator;
import junit.framework.Assert;
import net.emaze.dysfunctional.Dispatching;
import net.emaze.dysfunctional.Logic;
import net.emaze.dysfunctional.Reductions;
import net.emaze.dysfunctional.dispatching.delegates.Identity;
import net.emaze.dysfunctional.dispatching.logic.Predicate;
import org.junit.Test;

public class LongBitsIteratorTest {

    private static final Predicate<Boolean> IS_TRUE = Dispatching.predicate(new Identity<Boolean>());
    private static final Predicate<Boolean> IS_FALSE = Logic.not(IS_TRUE);
    private static final long MAX_LONG = 0xFFFFFFFFFFFFFFFFL;

    @Test
    public void zeroYieldsOnlyFalse() {
        final Iterator iterator = new LongBitsIterator(0L);
        Assert.assertTrue(Reductions.every(iterator, IS_FALSE));
    }
    
    @Test
    public void maxLongYieldsOnlyTrue() {
        final Iterator iterator = new LongBitsIterator(MAX_LONG);
        Assert.assertTrue(Reductions.every(iterator, IS_TRUE));
    }
    
    @Test
    public void iteratorContains64Elements() {
        final Iterator iterator = new LongBitsIterator(0L);
        Assert.assertEquals(64, Reductions.count(iterator));
    }

}
