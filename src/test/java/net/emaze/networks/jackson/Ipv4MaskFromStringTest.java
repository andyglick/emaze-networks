package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv4Mask;
import net.emaze.networks.jackson.Ipv4MaskToStringTest.BeanWithNetmask;
import org.junit.Test;

public class Ipv4MaskFromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'netmask':24}".replace("'", "\"");
        final Ipv4Mask expected = Ipv4Mask.net(24);
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithNetmask got = mapper.readValue(serialized, BeanWithNetmask.class);
        Assert.assertEquals(expected, got.getNetmask());
    }
}
